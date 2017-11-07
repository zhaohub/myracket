#lang racket/base

(require racket/list
         db)

(struct blog (db))

(struct post (blog id))

(define (initialize-blog! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
                (string-append
                 "create table posts "
                 "(id integer primary key, title text, body text)"))
    (blog-insert-post!
     the-blog "First Post" "This is my first post")
    (blog-insert-post!
     the-blog "Second Post" "This is another post"))
  (unless (table-exists? db "comments")
    (query-exec db
                "create table comments (pid integer, content text)")
    (post-insert-comment!
     the-blog (first (blog-posts the-blog))
     "First comment!"))
  the-blog)


(define (blog-insert-post! a-blog title body)
  (query-exec
   (blog-db a-blog)
   "insert into posts (title,body) values (?,?)"
   title body))

(define (post-insert-comment! a-blog p a-comment)
  (query-exec
   (blog-db a-blog)
   "insert into comments (pid,content) values (?,?)"
   (post-id p) a-comment))


(define (blog-posts a-blog)
  (define (id->post an-id)
    (post a-blog an-id))
  (map id->post
       (query-list
        (blog-db a-blog)
        "select id from posts")))


(define (post-title a-post)
  (query-value
   (blog-db (post-blog a-post))
   "select title from posts where id=?"
   (post-id a-post)))

(define (post-body p)
  (query-value
   (blog-db (post-blog p))
   "select body from posts where id=?"
   (post-id p)))

(define (post-comments p)
  (query-list
   (blog-db (post-blog p))
   "select content from comments where pid=?"
   (post-id p)))

(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)





