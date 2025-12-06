
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

INSERT INTO users (id, name) VALUES (1, 'Alice');
INSERT INTO users (id, name) VALUES (2, 'Bob');
INSERT INTO users (id, name) VALUES (3, 'Charlie');

-- Alice spent 150
INSERT INTO orders (user_id, amount) VALUES (1, 50.00);
INSERT INTO orders (user_id, amount) VALUES (1, 100.00);

-- Bob spent 80
INSERT INTO orders (user_id, amount) VALUES (2, 80.00);

-- Charlie spent 200
INSERT INTO orders (user_id, amount) VALUES (3, 200.00);
