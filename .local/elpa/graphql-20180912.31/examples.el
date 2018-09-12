(require 'subr-x)
(require 'json)
(require 'ghub)
(require 'graphql)

;;; The following functions create

(defun ghubp--graphql-submit (type object)
  (thread-last (graphql-encode object)
    (cons type)
    (list)
    (json-encode)
    (ghub-post "/graphql" nil)))
(defun ghubp-graphql-query (query)
  (ghubp--graphql-submit "query" (cons 'query query)))
(defun ghubp-graphql-mutation (mutation)
  (ghubp--graphql-submit "mutation" (cons 'mutation mutation)))

;;; Begin examples

(ghubp-graphql-query
 ;; Get the first hundred issues from vermiculus/magithub
 '((repository
    :arguments ((owner . "vermiculus")
                (name . "magithub"))
    (issues :arguments ((first . 100))
            (edges
             (node number title url id))))))
