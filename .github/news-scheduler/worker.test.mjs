import assert from 'node:assert/strict';
import { generateKeyPairSync, verify } from 'node:crypto';
import { test } from 'node:test';
import worker, { checkNews } from './worker.mjs';

const { privateKey, publicKey } = generateKeyPairSync('rsa', { modulusLength: 2048 });
const env = {
  GITHUB_APP_ID: '123',
  GITHUB_INSTALLATION_ID: '456',
  GITHUB_APP_PRIVATE_KEY: privateKey.export({ type: 'pkcs8', format: 'pem' }),
};
const now = new Date('2026-11-15T06:17:00Z');
const workflowPath = '/repos/lstehlik2809/People_Analytics_Blog/actions/workflows/people-analytics-news.yml';
const archivePath = '/lstehlik2809/People_Analytics_Blog/master/news_archive/2026-11-15.json';

function fakeGitHub({ archive = 404, activeStatus, dispatch = 200, fail } = {}) {
  const calls = [];
  async function request(input, options) {
    const { host, pathname, searchParams } = new URL(input);
    calls.push({ pathname, options });
    assert.equal(options.cache, 'no-store');
    assert.equal(options.redirect, 'manual');
    assert.ok(options.signal instanceof AbortSignal);
    if (host === 'raw.githubusercontent.com') {
      assert.equal(pathname, archivePath);
      assert.equal(options.method, 'HEAD');
      assert.equal(options.headers, undefined);
      return new Response(null, { status: archive });
    }

    assert.equal(host, 'api.github.com');
    assert.equal(options.headers['X-GitHub-Api-Version'], '2026-03-10');
    if (pathname === '/app/installations/456/access_tokens') {
      assert.equal(options.method, 'POST');
      assert.deepEqual(JSON.parse(options.body), { repositories: ['People_Analytics_Blog'], permissions: { actions: 'write' } });
      const [header, payload, signature] = options.headers.Authorization.slice('Bearer '.length).split('.');
      assert.deepEqual(JSON.parse(Buffer.from(header, 'base64url')), { alg: 'RS256', typ: 'JWT' });
      const claims = JSON.parse(Buffer.from(payload, 'base64url'));
      assert.equal(claims.iss, '123');
      assert.equal(claims.iat, now.getTime() / 1000 - 60);
      assert.ok(claims.exp > claims.iat && claims.exp <= now.getTime() / 1000 + 600);
      assert.ok(verify('RSA-SHA256', Buffer.from(`${header}.${payload}`), publicKey, Buffer.from(signature, 'base64url')));
      if (fail === 'token') return new Response('sensitive body', { status: 403 });
      return Response.json({ token: 'installation-token' }, { status: 201 });
    }

    assert.equal(options.headers.Authorization, 'Bearer installation-token');
    if (pathname === `${workflowPath}/runs`) {
      assert.equal(options.method, undefined);
      assert.equal(searchParams.get('branch'), 'master');
      assert.equal(searchParams.get('per_page'), '1');
      const status = searchParams.get('status');
      if (fail === 'runs' && status === 'queued') return new Response('sensitive body', { status: 503 });
      return Response.json({ workflow_runs: status === activeStatus ? [{ id: 7, status }] : [] });
    }

    assert.equal(pathname, `${workflowPath}/dispatches`);
    assert.equal(options.method, 'POST');
    assert.deepEqual(JSON.parse(options.body), { ref: 'master' });
    const body = dispatch === 200 ? JSON.stringify({ workflow_run_id: 900 }) : null;
    return new Response(body, { status: dispatch });
  }
  return { calls, request };
}

test('the cron worker only exposes a scheduled handler', () => {
  assert.deepEqual(Object.keys(worker), ['scheduled']);
});

test('uses Prague date around midnight and skips the minute before 07:17 in winter and summer', async () => {
  const times = [
    ['2026-11-14T23:05:00Z', '2026-11-15'],
    ['2026-11-15T06:16:59Z', '2026-11-15'],
    ['2026-07-15T05:16:59Z', '2026-07-15'],
  ];
  for (const [utc, date] of times) {
    const request = () => assert.fail(`Unexpected API request for ${utc}`);
    assert.deepEqual(await checkNews({}, { now: new Date(utc), request }), { date, outcome: 'before-window' });
  }
});

test('at 07:17 stops after an existing archive, without issuing a token', async () => {
  const { calls, request } = fakeGitHub({ archive: 200 });
  assert.deepEqual(await checkNews(env, { now, request }), { date: '2026-11-15', outcome: 'archive-present' });
  assert.equal(calls.length, 1);
  const summer = new Date('2026-07-15T05:17:00Z');
  assert.deepEqual(await checkNews(env, { now: summer, request: (url) => {
    assert.ok(url.endsWith('/news_archive/2026-07-15.json'));
    return new Response(null, { status: 200 });
  } }), { date: '2026-07-15', outcome: 'archive-present' });
});

test('checks credentials before accessing GitHub in the collection window', async () => {
  await assert.rejects(checkNews({}, { now, request: () => assert.fail('Unexpected API request') }), /Configure GITHUB_APP_ID/);
  const { calls, request } = fakeGitHub();
  await assert.rejects(checkNews({ ...env, GITHUB_APP_PRIVATE_KEY: 'invalid key' }, { now, request }), /must be a PKCS#8 PEM/);
  assert.equal(calls.length, 1);
});

test('only archive HTTP 404 can lead to dispatch', async () => {
  for (const status of [301, 403, 429, 503]) {
    const { calls, request } = fakeGitHub({ archive: status });
    await assert.rejects(checkNews(env, { now, request }), new RegExp(`Archive check returned HTTP ${status}`));
    assert.equal(calls.length, 1);
  }
  const request = () => { throw new Error('sensitive body'); };
  await assert.rejects(checkNews(env, { now, request }), { message: 'Archive check request failed or timed out' });
});

test('suppresses dispatch for each active GitHub Actions state', async () => {
  for (const activeStatus of ['waiting', 'queued', 'pending', 'requested', 'in_progress']) {
    const { calls, request } = fakeGitHub({ activeStatus });
    assert.deepEqual(await checkNews(env, { now, request }), { date: '2026-11-15', outcome: 'run-active' });
    assert.ok(calls.every(({ pathname }) => !pathname.endsWith('/dispatches')));
  }
});

test('dispatches exactly once with a scoped token when today has no archive or active run', async () => {
  const { calls, request } = fakeGitHub();
  assert.deepEqual(await checkNews(env, { now, request }), { date: '2026-11-15', outcome: 'dispatched', runId: 900 });
  assert.equal(calls.filter(({ pathname }) => pathname.endsWith('/dispatches')).length, 1);
  assert.equal(calls.at(-1).pathname, `${workflowPath}/dispatches`);
  const legacy = fakeGitHub({ dispatch: 204 });
  assert.deepEqual(await checkNews(env, { now, request: legacy.request }), { date: '2026-11-15', outcome: 'dispatched' });
});

test('fails closed for token and run lookup errors and surfaces dispatch rejection', async () => {
  for (const [fail, message] of [['token', 'Installation token returned HTTP 403'], ['runs', 'Check queued runs returned HTTP 503']]) {
    const { calls, request } = fakeGitHub({ fail });
    await assert.rejects(checkNews(env, { now, request }), { message });
    assert.ok(calls.every(({ pathname }) => !pathname.endsWith('/dispatches')));
  }
  const { request } = fakeGitHub({ dispatch: 422 });
  await assert.rejects(checkNews(env, { now, request }), { message: 'Workflow dispatch returned HTTP 422' });
});
