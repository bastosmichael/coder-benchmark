You are a senior TypeScript engineer.

Implement the following API in `src/lruCache.ts`:

```ts
export class LRUCache<K, V> {
  constructor(options: { capacity: number; ttlMs?: number });
  get(key: K): V | undefined;
  set(key: K, value: V): void;
  has(key: K): boolean;
  delete(key: K): boolean;
  size(): number;
}
```

Requirements:
- Pure TypeScript targeting ES2020.
- Do not use `any` or `@ts-ignore`.
- `get` and `set` should be O(1) on average.
- If `ttlMs` is provided, lazily evict expired entries during `get` or `set`.

Output rules (important for our extraction logic):
- Respond with a single fenced code block using language `ts`.
- The code block must contain **only** the full contents of `src/lruCache.ts` and nothing else outside the fence.
