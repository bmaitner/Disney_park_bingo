// Offline outbox for anything sent to the Apps Script inbox in
// backend/inbox.gs (suggestions and card results). Items wait on the
// phone until they're delivered.

const MAX_ATTEMPTS = 20;

// Errors worth retrying later rather than dropping. "unknown request type"
// means the deployed script predates this kind of submission.
const RETRY_ERRORS = ["busy", "unknown request type"];

const DEVICE_KEY = "parkbingo.device";

// A random id kept on this phone so repeat visitors can be told apart from new
// ones. It's only sent with usage counts, never with suggestions or results.
// Returns undefined if storage is unavailable (e.g. some private windows).
export function deviceId(storage = globalThis.localStorage) {
  try {
    let id = storage.getItem(DEVICE_KEY);
    if (!/^[A-Za-z0-9-]{8,64}$/.test(id ?? "")) {
      id = newId();
      storage.setItem(DEVICE_KEY, id);
    }
    return id;
  } catch {
    return undefined;
  }
}

// Fire-and-forget usage count for the inbox's "visits" and "visitors" tabs:
// event is "open", "card" (dealt in the app), or "print" (with n cards).
// Counts aren't retried, and local development isn't counted.
export function sendCount(url, event, n = 1, fetchImpl = fetch,
                          host = globalThis.location?.hostname, device = deviceId()) {
  if (!url || ["localhost", "127.0.0.1"].includes(host)) return;
  fetchImpl(url, {
    method: "POST",
    headers: { "Content-Type": "text/plain;charset=utf-8" },
    body: JSON.stringify({ type: "visit", event, n, device }),
  }).catch(() => {});
}

export function newId() {
  if (globalThis.crypto?.randomUUID) return crypto.randomUUID();
  return `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 12)}`;
}

// Try to send every queued item. Returns the items still waiting.
// Items the inbox rejects as invalid are dropped; network failures and
// retryable errors stay queued (the inbox ignores duplicates, so retries are
// safe).
export async function flushOutbox(outbox, url, fetchImpl = fetch) {
  const remaining = [];
  for (const item of outbox) {
    let keep = true;
    try {
      const response = await fetchImpl(url, {
        method: "POST",
        // text/plain avoids a CORS preflight, which Apps Script can't answer.
        headers: { "Content-Type": "text/plain;charset=utf-8" },
        body: JSON.stringify(item.payload),
      });
      const result = await response.json();
      keep = !result.ok && (Boolean(result.retry) || RETRY_ERRORS.includes(result.error));
    } catch {
      keep = true;
    }
    if (keep) {
      const attempts = (item.attempts ?? 0) + 1;
      if (attempts < MAX_ATTEMPTS) remaining.push({ ...item, attempts });
    }
  }
  return remaining;
}
