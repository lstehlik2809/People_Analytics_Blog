const owner = 'lstehlik2809';
const repository = 'People_Analytics_Blog';
const branch = 'master';
const workflow = 'people-analytics-news.yml';
const workflowPath = `/repos/${owner}/${repository}/actions/workflows/${workflow}`;
const activeStatuses = ['waiting', 'queued', 'pending', 'requested', 'in_progress'];
const pragueTime = new Intl.DateTimeFormat('en-GB', {
  timeZone: 'Europe/Prague',
  year: 'numeric',
  month: '2-digit',
  day: '2-digit',
  hour: '2-digit',
  minute: '2-digit',
  hourCycle: 'h23',
});

function base64url(bytes) {
  return btoa(String.fromCharCode(...bytes)).replaceAll('+', '-').replaceAll('/', '_').replaceAll('=', '');
}

async function appJwt(appId, pem, now) {
  const encode = (value) => base64url(new TextEncoder().encode(JSON.stringify(value)));
  const seconds = Math.floor(now.getTime() / 1000);
  const claims = { iat: seconds - 60, exp: seconds + 540, iss: appId };
  const message = `${encode({ alg: 'RS256', typ: 'JWT' })}.${encode(claims)}`;
  const pkcs8 = pem.trim().match(/^-----BEGIN PRIVATE KEY-----([A-Za-z0-9+/=\s]+)-----END PRIVATE KEY-----$/);
  if (!pkcs8) throw new Error('GITHUB_APP_PRIVATE_KEY must be a PKCS#8 PEM');

  try {
    const bytes = Uint8Array.from(atob(pkcs8[1]), (character) => character.charCodeAt(0));
    const key = await crypto.subtle.importKey('pkcs8', bytes,
      { name: 'RSASSA-PKCS1-v1_5', hash: 'SHA-256' }, false, ['sign']);
    const signature = await crypto.subtle.sign(key.algorithm.name, key, new TextEncoder().encode(message));
    return `${message}.${base64url(new Uint8Array(signature))}`;
  } catch {
    throw new Error('Could not sign with GITHUB_APP_PRIVATE_KEY');
  }
}

async function send(request, label, url, options) {
  try {
    return await request(url, { ...options, cache: 'no-store', redirect: 'manual', signal: AbortSignal.timeout(10_000) });
  } catch {
    throw new Error(`${label} request failed or timed out`);
  }
}

async function github(request, label, path, token, body) {
  const options = {
    headers: {
      Accept: 'application/vnd.github+json',
      Authorization: `Bearer ${token}`,
      'User-Agent': 'people-analytics-news-scheduler',
      'X-GitHub-Api-Version': '2026-03-10',
    },
  };
  if (body !== undefined) {
    options.method = 'POST';
    options.headers['Content-Type'] = 'application/json';
    options.body = JSON.stringify(body);
  }
  const response = await send(request, label, `https://api.github.com${path}`, options);
  if (!response.ok) throw new Error(`${label} returned HTTP ${response.status}`);
  if (response.status === 204) return null;
  try {
    return await response.json();
  } catch {
    throw new Error(`${label} returned invalid JSON`);
  }
}

export async function checkNews(env, { now = new Date(), request = fetch } = {}) {
  const parts = Object.fromEntries(pragueTime.formatToParts(now).map(({ type, value }) => [type, value]));
  const date = `${parts.year}-${parts.month}-${parts.day}`;
  if (`${parts.hour}:${parts.minute}` < '07:17') return { date, outcome: 'before-window' };

  const { GITHUB_APP_ID: appId, GITHUB_INSTALLATION_ID: installationId, GITHUB_APP_PRIVATE_KEY: key } = env;
  if (!/^\d+$/.test(appId) || !/^\d+$/.test(installationId) || !key?.trim()) {
    throw new Error('Configure GITHUB_APP_ID, GITHUB_INSTALLATION_ID, and GITHUB_APP_PRIVATE_KEY');
  }

  const archiveUrl = `https://raw.githubusercontent.com/${owner}/${repository}/${branch}/news_archive/${date}.json`;
  const archive = await send(request, 'Archive check', archiveUrl, { method: 'HEAD' });
  if (archive.status === 200) return { date, outcome: 'archive-present' };
  if (archive.status !== 404) throw new Error(`Archive check returned HTTP ${archive.status}`);

  const jwt = await appJwt(appId, key, now);
  const installation = await github(request, 'Installation token', `/app/installations/${installationId}/access_tokens`, jwt, {
    repositories: [repository],
    permissions: { actions: 'write' },
  });
  if (!installation?.token) throw new Error('GitHub did not return an installation token');

  const runChecks = activeStatuses.map(async (status) => {
    const query = new URLSearchParams({ branch, status, per_page: '1' });
    const result = await github(request, `Check ${status} runs`, `${workflowPath}/runs?${query}`, installation.token);
    if (!Array.isArray(result?.workflow_runs)) throw new Error(`Check ${status} runs returned no run list`);
    return result.workflow_runs.length > 0;
  });
  if ((await Promise.all(runChecks)).some(Boolean)) return { date, outcome: 'run-active' };

  const dispatch = await github(request, 'Workflow dispatch', `${workflowPath}/dispatches`, installation.token, { ref: branch });
  const result = { date, outcome: 'dispatched' };
  if (Number.isSafeInteger(dispatch?.workflow_run_id)) result.runId = dispatch.workflow_run_id;
  return result;
}

export default {
  async scheduled(_controller, env) {
    console.info({ event: 'news-schedule', ...await checkNews(env) });
  },
};
