#lang web-server/insta

(require mzlib/file)
(require "model-2.rkt")

(define (start request)
  (render-blog-page
   (initialize-blog!
    (build-absolute-path "/home/zhaogang/gitrep/myracket"
                "the-blog-data.db"))
   request))

(define (render-blog-page a-blog request)
  (define (response-generator embed/url)
  (response/xexpr
   `(html (head (title "My Blog")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
          (body (h1 "My blog")
                ,(render-posts a-blog embed/url)
                (form ((action
                        ,(embed/url insert-post-handler)))
                 (div (input ((name "title"))))
                 (div (input ((name "body"))))
                 (div (input ((type "submit")
                         (value "ADD")))))))))
  (define (insert-post-handler request)
    (define bindings (request-bindings request))
    (blog-insert-post!
     a-blog
     (extract-binding/single 'title bindings)
     (extract-binding/single 'body bindings))
    (render-blog-page a-blog (redirect/get)))
  (send/suspend/dispatch response-generator))

(static-files-path "htdocs")

(define (render-post-detail-page a-blog a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Post Detail"))
            (body
             (h1 "Post Details")
             (h2 ,(post-body a-post))
             ,(render-as-itemized-list
               (post-comments a-post))
             (form ((action
                     ,(embed/url insert-comment-handler)))
                   (input ((name "comment")))
                   (input ((type "submit"))))
             (a ((href ,(embed/url back-handler)))
                "Back to the blog")))))
  (define (parse-comment bindings)
    (extract-binding/single 'comment bindings))

  (define (insert-comment-handler request)
    (render-confirm-add-comment-page
     a-blog
     (parse-comment (request-bindings request))
     a-post
     request))

  (define (back-handler request)
    (render-blog-page a-blog request))
  
  (send/suspend/dispatch response-generator))

(define (render-confirm-add-comment-page a-blog a-comment a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Add Comment"))
            (body
             (h1 "Add Comment")
             "The comment:" (div (p ,a-comment))
             "will be added to "
             (div ,(post-title a-post))

             (p (a ((href ,(embed/url yes-handler)))
                   "Yes, add the comment."))
             (p (a ((href ,(embed/url cancel-handler)))
                   "NO, I changed my mind!"))))))
  (define (yes-handler request)
    (post-insert-comment! a-blog a-post a-comment)
    (render-post-detail-page a-blog a-post request))

  (define (cancel-handler request)
    (render-post-detail-page a-blog a-post request))

  (send/suspend/dispatch response-generator))

(define (render-post a-blog a-post embed/url)
  (define (view-post-handler request)
    (render-post-detail-page a-blog a-post request))
  `(div ((class "post"))
        (a ((href ,(embed/url view-post-handler))
            )
           ,(post-title a-post))
        (div ((class "content"))
             ,(post-body a-post))
        (div ((class "comments"))
             (span ((class "hot")) ,(number->string (length (post-comments a-post))))
             " comment(s)")))

(define (render-posts a-blog embed/url)
  (define (render-post/embed/url a-post)
    (render-post a-blog a-post embed/url))
  `(div ((class "posts"))
        ,@(map render-post/embed/url (blog-posts a-blog))))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))


