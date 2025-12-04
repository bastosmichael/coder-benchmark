import { describe, expect, it } from 'vitest';
import { LRUCache } from '../src/lruCache';

describe('LRUCache', () => {
  it('evicts the least recently used item when capacity is exceeded', () => {
    const cache = new LRUCache<string, number>({ capacity: 2 });
    cache.set('a', 1);
    cache.set('b', 2);

    expect(cache.get('a')).toBe(1);
    cache.set('c', 3);

    expect(cache.get('b')).toBeUndefined();
    expect(cache.get('a')).toBe(1);
    expect(cache.get('c')).toBe(3);
  });

  it('lazily expires entries when ttl is provided', async () => {
    const cache = new LRUCache<string, number>({ capacity: 2, ttlMs: 10 });
    cache.set('temp', 42);

    expect(cache.get('temp')).toBe(42);
    await new Promise((resolve) => setTimeout(resolve, 20));

    expect(cache.get('temp')).toBeUndefined();
  });
});
