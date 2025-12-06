
You are a SQL Expert.

Given the following schema (SQLite):

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT UNIQUE
);

CREATE TABLE orders (
    id INTEGER PRIMARY KEY,
    user_id INTEGER,
    amount DECIMAL(10, 2),
    created_at DATETIME,
    FOREIGN KEY(user_id) REFERENCES users(id)
);
```

Write a SQL script `src/queries.sql` that:
1.  Creates a VIEW named `HighSpenders`.
2.  The view should return `user_id`, `name`, and `total_spent` (sum of order amounts).
3.  Include only users who have spent more than 100.00.
4.  Order by `total_spent` DESC.

Output full content of `src/queries.sql`.
