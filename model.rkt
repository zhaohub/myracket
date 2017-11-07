#lang racket/base

(struct blog (posts) #:mutable #:prefab)

(struct post (title body comments) #:mutable)

(define BLOG
  (blog
   (list (post "SecondPost"
               "This is another post"
               (list))
         (post "FirstPost"
               "This is my first post"
               (list "First comment!")))))

(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

(define (post-insert-comment! a-post a-comment)
  (set-post-comments!
   a-post
   (append (post-comments a-post) (list a-comment))))


(provide (all-defined-out))