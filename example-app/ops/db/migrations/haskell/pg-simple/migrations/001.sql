SET TIME ZONE 'UTC';

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE users (
  id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  email text NOT NULL,
  password text NOT NULL
);

INSERT INTO users (email, password)
VALUES ('haskman@hasktar.com', crypt('new password', gen_salt('bf')));
