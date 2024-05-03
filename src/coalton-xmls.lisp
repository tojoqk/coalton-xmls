(defpackage #:coalton-xmls
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:list #:coalton-library/list))
  (:export #:Node
           #:NativeNode))

(in-package #:coalton-xmls)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :native xmls:node)
  (define-type NativeNode)

  (define-struct Node
    (name String)
    (ns (Optional String))
    (attrs (List (Tuple String String)))
    (children (List Node)))

  (define-instance (Into Node NativeNode)
    (define (into (Node name ns attrs children))
      (let ((declare native-children (List NativeNode))
            (native-children (map into children))
            (declare native-attrs (List (List String)))
            (native-attrs (map (fn ((Tuple k v)) (make-list k v)) attrs)))
        (match ns
          ((Some native-ns)
           (lisp NativeNode (name native-ns native-attrs native-children)
             (xmls:make-node :name name
                             :ns native-ns
                             :attrs native-attrs
                             :children native-children)))
          ((None)
           (lisp NativeNode (name native-attrs native-children)
             (xmls:make-node :name name
                             :ns cl:nil
                             :attrs native-attrs
                             :children native-children)))))))
  
  (define-instance (Into NativeNode Node)
    (define (into n)
      (Node (lisp String (n) (xmls:node-name n))
            (lisp (Optional String) (n)
              (alexandria:if-let (result (xmls:node-ns n))
                (Some result)
                None))
            (map (fn (x)
                   (Tuple (list:car x)
                          (list:car (list:cdr x))))
                 (lisp (List (List String)) (n) (xmls:node-attrs n)))
            (map into
                 (lisp (List NativeNode) (n)
                   (xmls:node-children n))))))

  (define-instance (Iso Node NativeNode))

  (declare native-parse (String -> (Optional NativeNode)))
  (define (native-parse s)
    (lisp (Optional NativeNode) (s)
      (alexandria:if-let (result (xmls:parse s))
        (Some result)
        None)))

  (declare parse (String -> (Optional Node)))
  (define (parse s)
    (map into (native-parse s)))

  (define-instance (tryInto String Node)
    (define (tryInto s)
      (match (parse s)
        ((None) (Err (<> "Cannot parse string as xml: " s)))
        ((Some n) (Ok n)))))

  (repr :native cl:stream)
  (define-type StringOutputStream)

  (declare make-string-output-stream (Unit -> StringOutputStream))
  (define (make-string-output-stream)
    (lisp StringOutputStream ()
      (cl:make-string-output-stream)))

  (declare get-output-stream-string (StringOutputStream -> String))
  (define (get-output-stream-string s)
    (lisp String (s)
      (cl:get-output-stream-string s)))

  (declare write-xml (NativeNode -> StringOutputStream -> Unit))
  (define (write-xml n s)
    (lisp Unit (n s)
      (xmls:write-xml n s)
      Unit))

  (define-instance (Into Node String)
    (define (into n)
      (let s = (make-string-output-stream))
      (write-xml (into n) s)
      (get-output-stream-string s)))
  )
