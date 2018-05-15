SET TIME ZONE 'UTC';

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE users (
  id uuid NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY,
  email text NOT NULL,
  password text NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);

INSERT INTO users (email, password, updated_at, created_at)
VALUES 
  ('admin@haskstar.com'
  , '14|8|1|5CmOii8RilPuv6vDZFFcPDX8WyfYKnhV+eYcRDwadvM=|bcvjyIY/O3PUZpeorOLqGPOLM2ind2fuEvY1B4ATrAJQGhNqNFWjxRs0W51zxrwMxE4imhhA9Fn1lTSgqkWvkQ=='
  , now()
  , DEFAULT
  );
