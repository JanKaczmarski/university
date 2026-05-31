## Ray Actor API  ActorPool (extra credit)

`ray.util.ActorPool` is used in `client.py`  this is a Ray feature **not covered in labs**.

### Where and why

**Upload**  `ActorPool(chunk_info.nodes)` + `pool.map()` distributes the same chunk data to all replica DataNodes in the pool:
```python
pool = ActorPool(chunk_info.nodes)
list(pool.map(lambda actor, d: actor.write_chunk.remote(chunk_id, d), [data] * len(nodes)))
```

**Download**  scatter-gather read across all replicas. All replicas are queried in parallel, the first successful response is returned immediately (no waiting for slow/failed nodes):
```python
pool = ActorPool(chunk_info.nodes)
for _ in chunk_info.nodes:
    pool.submit(lambda actor, cid: actor.read_chunk.remote(cid), chunk_id)
while pool.has_next():
    return pool.get_next_unordered()  # first replica to respond wins
```

This is genuinely better than sequential fallback  if `dn-0` is slow or failed, `dn-1` can respond first without waiting for `dn-0` to time out.
