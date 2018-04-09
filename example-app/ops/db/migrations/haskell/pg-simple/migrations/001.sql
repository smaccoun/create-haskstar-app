SET TIME ZONE 'UTC';

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE users (
  id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  email text NOT NULL,
  password text NOT NULL
);

INSERT INTO users (email, password)
VALUES ('haskman@hasktar.com', '14|8|1|O07xqtvhYQrcuMvBFTY/jP/MuXRFs1qZW2gmBdNIBbc=|TCJXrS9mYRxS+ehscGynilbvdgylCojycY2WnTSd8+HlXdTZq4MCY0EtuSt3ZvojIx7UUdVhyUjBnzTOQsGaig==');
