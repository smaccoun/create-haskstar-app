CREATE TABLE public.blog_posts
(
  id UUID DEFAULT gen_random_uuid() PRIMARY KEY NOT NULL,
  title TEXT NOT NULL,
  content TEXT,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
