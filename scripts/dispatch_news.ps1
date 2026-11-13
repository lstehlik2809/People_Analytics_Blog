$ErrorActionPreference = 'Stop'
$repo = 'lstehlik2809/People_Analytics_Blog'
$branch = 'master'
$workflow = 'people-analytics-news.yml'
$gh = (Get-Command gh.exe).Source
$prague = [TimeZoneInfo]::FindSystemTimeZoneById('Central Europe Standard Time')
$day = [TimeZoneInfo]::ConvertTimeFromUtc([DateTime]::UtcNow, $prague).ToString('yyyy-MM-dd')

$file = "https://raw.githubusercontent.com/$repo/$branch/news_archive/$day.json"
try {
    $request = Invoke-WebRequest -Uri $file -UseBasicParsing -Headers @{'Cache-Control' = 'no-cache'} -TimeoutSec 20
    $edition = $request.Content | ConvertFrom-Json
    if ($edition.date -ne $day) { throw 'GitHub returned an unexpected News archive date.' }
    Write-Output "The News edition for $day is already saved."
    exit 0
} catch {
    if (-not $_.Exception.Response -or [int]$_.Exception.Response.StatusCode -ne 404) { throw }
}

$activeRuns = & $gh run list --repo $repo --workflow $workflow --branch $branch --limit 5 --json status --jq '[.[] | select(.status != "completed")] | length'
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
if ([int]$activeRuns -gt 0) {
    Write-Output 'A News run is already active.'
    exit 0
}

& $gh workflow run $workflow --repo $repo --ref $branch
exit $LASTEXITCODE
