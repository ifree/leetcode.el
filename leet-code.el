;;; leet-code.el --- leet-code client for emacs

;;; Commentary:
;; 
;; login, submit, check result, retrieve quest info

;;; Code:

(eval-when-compile (require 'cl))

(require 'request)
(require 'json )

(defconst leet-code-base-url "https://oj.leetcode.com")
(defconst leet-code-action-login "accounts/login/")
(defconst leet-code-action-logout "accounts/logout/")
(defconst leet-code-request-header
  '(("Referer" . "https://oj.leetcode.com/")
    ("Host" . "oj.leetcode.com")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:35.0) Gecko/20100101 Firefox/35.0")))
(defconst leet-code-csrf-key "csrfmiddlewaretoken")
(defconst leet-code-csrf-cookie-key "csrftoken")
(defvar *leet-code-last-submission-id* nil)

;;; url-retrieve is good at dealing with cookie
(setq request-backend 'url-retrieve)


(defun leet-code--file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst leet-code-quests '(("two-sum"  1) 
                             ("add-two-numbers"  2) 
                             ("longest-substring-without-repeating-characters"  3) 
                             ("median-of-two-sorted-arrays"  4) 
                             ("longest-palindromic-substring"  5) 
                             ("zigzag-conversion"  6) 
                             ("reverse-integer"  7) 
                             ("string-to-integer-atoi"  8) 
                             ("palindrome-number"  9) 
                             ("regular-expression-matching"  10) 
                             ("container-with-most-water"  11) 
                             ("integer-to-roman"  12) 
                             ("roman-to-integer"  13) 
                             ("longest-common-prefix"  14) 
                             ("3sum"  15) 
                             ("3sum-closest"  16) 
                             ("4sum"  17) 
                             ("letter-combinations-of-a-phone-number"  18) 
                             ("remove-nth-node-from-end-of-list"  19) 
                             ("valid-parentheses"  20) 
                             ("merge-two-sorted-lists"  21) 
                             ("generate-parentheses"  22) 
                             ("merge-k-sorted-lists"  23) 
                             ("swap-nodes-in-pairs"  24) 
                             ("reverse-nodes-in-k-group"  25) 
                             ("remove-duplicates-from-sorted-array"  26) 
                             ("remove-element"  27) 
                             ("implement-strstr"  28) 
                             ("divide-two-integers"  29) 
                             ("substring-with-concatenation-of-all-words"  30) 
                             ("next-permutation"  31) 
                             ("longest-valid-parentheses"  32) 
                             ("search-in-rotated-sorted-array"  33) 
                             ("search-for-a-range"  34) 
                             ("search-insert-position"  35) 
                             ("valid-sudoku"  36) 
                             ("sudoku-solver"  37) 
                             ("count-and-say"  38) 
                             ("combination-sum"  39) 
                             ("combination-sum-ii"  40) 
                             ("first-missing-positive"  41) 
                             ("trapping-rain-water"  42) 
                             ("multiply-strings"  43) 
                             ("wildcard-matching"  44) 
                             ("jump-game-ii"  45) 
                             ("permutations"  46) 
                             ("permutations-ii"  47) 
                             ("rotate-image"  48) 
                             ("anagrams"  49) 
                             ("powx-n"  50) 
                             ("n-queens"  51) 
                             ("n-queens-ii"  52) 
                             ("maximum-subarray"  53) 
                             ("spiral-matrix"  54) 
              p               ("jump-game"  55) 
                             ("merge-intervals"  56) 
                             ("insert-interval"  57) 
                             ("length-of-last-word"  58) 
                             ("spiral-matrix-ii"  59) 
                             ("permutation-sequence"  60) 
                             ("rotate-list"  61) 
                             ("unique-paths"  62) 
                             ("unique-paths-ii"  63) 
                             ("minimum-path-sum"  64) 
                             ("valid-number"  65) 
                             ("plus-one"  66) 
                             ("add-binary"  67) 
                             ("text-justification"  68) 
                             ("sqrtx"  69) 
                             ("climbing-stairs"  70) 
                             ("simplify-path"  71) 
                             ("edit-distance"  72) 
                             ("set-matrix-zeroes"  73) 
                             ("search-a-2d-matrix"  74) 
                             ("sort-colors"  75) 
                             ("minimum-window-substring"  76) 
                             ("combinations"  77) 
                             ("subsets"  78) 
                             ("word-search"  79) 
                             ("remove-duplicates-from-sorted-array-ii"  80) 
                             ("search-in-rotated-sorted-array-ii"  81) 
                             ("remove-duplicates-from-sorted-list-ii"  82) 
                             ("remove-duplicates-from-sorted-list"  83) 
                             ("largest-rectangle-in-histogram"  84) 
                             ("maximal-rectangle"  85) 
                             ("partition-list"  86) 
                             ("scramble-string"  87) 
                             ("merge-sorted-array"  88) 
                             ("gray-code"  89) 
                             ("subsets-ii"  90) 
                             ("decode-ways"  91) 
                             ("reverse-linked-list-ii"  92) 
                             ("restore-ip-addresses"  93) 
                             ("binary-tree-inorder-traversal"  94) 
                             ("unique-binary-search-trees-ii"  95) 
                             ("unique-binary-search-trees"  96) 
                             ("interleaving-string"  97) 
                             ("validate-binary-search-tree"  98) 
                             ("recover-binary-search-tree"  99) 
                             ("same-tree"  100) 
                             ("symmetric-tree"  101) 
                             ("binary-tree-level-order-traversal"  102) 
                             ("binary-tree-zigzag-level-order-traversal"  103) 
                             ("maximum-depth-of-binary-tree"  104) 
                             ("construct-binary-tree-from-preorder-and-inorder-traversal"  105) 
                             ("construct-binary-tree-from-inorder-and-postorder-traversal"  106) 
                             ("binary-tree-level-order-traversal-ii"  107) 
                             ("convert-sorted-array-to-binary-search-tree"  108) 
                             ("convert-sorted-list-to-binary-search-tree"  109) 
                             ("balanced-binary-tree"  110) 
                             ("minimum-depth-of-binary-tree"  111) 
                             ("path-sum"  112) 
                             ("path-sum-ii"  113) 
                             ("flatten-binary-tree-to-linked-list"  114) 
                             ("distinct-subsequences"  115) 
                             ("populating-next-right-pointers-in-each-node"  116) 
                             ("populating-next-right-pointers-in-each-node-ii"  117) 
                             ("pascals-triangle"  118) 
                             ("pascals-triangle-ii"  119) 
                             ("triangle"  120) 
                             ("best-time-to-buy-and-sell-stock"  121) 
                             ("best-time-to-buy-and-sell-stock-ii"  122) 
                             ("best-time-to-buy-and-sell-stock-iii"  123) 
                             ("binary-tree-maximum-path-sum"  124) 
                             ("valid-palindrome"  125) 
                             ("word-ladder-ii"  126) 
                             ("word-ladder"  127) 
                             ("longest-consecutive-sequence"  128) 
                             ("sum-root-to-leaf-numbers"  129) 
                             ("surrounded-regions"  130) 
                             ("palindrome-partitioning"  131) 
                             ("palindrome-partitioning"  132)))

(defconst leet-code-status-map
  '((10 "AC" )
   (11 "WA" )
   (12 "MLE" )
   (13 "OLE" )
   (14 "TLE" )
   (15 "RE" )
   (16 "IE" )
   (20 "CE" )
   (21 "UE" )))


(defun leet-code--fresh-request ()
  (if (eq request-backend 'url-retrieve)
      (url-cookie-clean-up)
    (when (file-exists-p (request--curl-cookie-jar))
      (delete-file (request--curl-cookie-jar))))

  (request leet-code-base-url
           :sync t
           :headers leet-code-request-header
           ))

(defun leet-code--login (uname pwd)
  (interactive
   (list
    (read-string "input username or email ")
    (read-passwd "intpu password ")
    )
   )
  (unless (leet-code--is-loggedin)
    (leet-code--fresh-request)
    (request (concat leet-code-base-url "/" leet-code-action-login)
             :type "POST"
             :data `(("login" . ,uname )
                     ("password" . ,pwd)
                     ("Host" . "oj.leetcode.com")
                     (,leet-code-csrf-key .
                                          ,(cdr
                                            (assoc
                                             leet-code-csrf-cookie-key
                                             (request-cookie-alist
                                              "oj.leetcode.com" "/" t)))))
             :headers leet-code-request-header)))

(defun leet-code--is-loggedin ()
  (string-match "Successfully signed in"
                (or (cdr
                     (assoc
                      "messages"
                      (request-cookie-alist "oj.leetcode.com" "/" t))) "")))


(defun leet-code--logout ()
  (interactive)
  (request (concat leet-code-base-url "/" leet-code-action-logout)
           :sync t
           :headers leet-code-request-header))


(defun* leet-code--submit
    (content &optional qname qid &key (type "large") (lang "cpp"))  
  (assert (leet-code--is-loggedin) t "login first")
  (unless (and qname qid)
    (let ((quest
           (assoc
            (completing-read "choose quest: " leet-code-quests nil t) 
            leet-code-quests
            )))
      (if quest
          (setq qname (car quest)  qid (cadr quest))
          (error "invalid quest"))
      )
    )
   
   (request
    (concat
     leet-code-base-url
     "/problems/" qname "/submit/" )
    :type "POST"
    :data `((,leet-code-csrf-key .
                                 ,(cdr (assoc leet-code-csrf-cookie-key
                                              (request-cookie-alist "oj.leetcode.com" "/" t)) )
                                 )
            ("lang" . ,lang)
            ("data_input" . "")
            ("question_id" . ,(number-to-string qid))
            ("judge_type" . ,type);large, small ?
            ("typed_code" . ,content)
            )
    :headers leet-code-request-header
    :parser 'json-read
    :success
    (function* (lambda (&key data &allow-other-keys)
                 (setq *leet-code-last-submission-id*
                       (number-to-string
                        (cdr (assoc 'submission_id data))))
                 (leet-code--check-submission)))))


(defun leet-code--submit-region (start end)
  (interactive "r")
  (leet-code--submit (buffer-substring start end)))


(defun leet-code--submit-file (file)
  (interactive (list
                (read-file-name
                 "select file: "
                 default-directory
                 (file-name-nondirectory buffer-file-name))))
  (leet-code--submit (leet-code--file-to-string file)))



(defun leet-code--check-submission (&optional sid)
  (assert (leet-code--is-loggedin) t "login first")
  (unless sid
    (setq sid *leet-code-last-submission-id*))
  (request
   (concat leet-code-base-url "/submissions/detail/" sid "/check/")
   :parser 'json-read
   :headers leet-code-request-header
   :success
   (function* (lambda (&key data &allow-other-keys)
                (let ((status-code (cdr (assoc 'status_code data)))
                      (state (cdr (assoc 'state data)))
                      )
                  (if (not (string= state "SUCCESS"))
                      (run-with-idle-timer
                       3
                       nil
                       'leet-code--check-submission)
                      (message (cadr
                            (assoc
                             status-code
                             leet-code-status-map)))))))))

(defun leet-code--parse-code (raw-content)
  (let ((re
         ;; I don't want to do this... but they don't provide me some API
         "<div\s*?class=\"col-md-12\"\s*?ng-init=\"init(\\(.*\\),\s*?'\\w+');\">"
         )
        code-str
        code-obj)
    (string-match re raw-content)
    (setq code-str (match-string-no-properties 1 raw-content))
    (with-temp-buffer
      (insert code-str)
      (while (search-backward "'" (point-min) t)
        (replace-match "\""))
      (goto-char 0)
      (setq code-obj (json-read))
      )
    (when (not (stringp code-obj))      ;no error thrown and result isn't str.
      (let ((cpp-entry                  ;why hard code? java devs use emacs are
                                        ;rarely, if you want other lang, send me
                                        ;PR or raise a issue. 
             (car (delq nil
                        (mapcar (lambda (entry)
                                  (and
                                   (string= (cdr (assoc 'value entry)) "cpp" )
                                   entry)) code-obj)))))
        (cdr
         (assoc 'defaultCode cpp-entry))))))

(defun leet-code--parse-info (raw-content)
  (substring-no-properties
   raw-content
   (string-match "<div\s?class=\"question-content\">" raw-content) ;malformatted
                                                                   ;html? it's
                                                                   ;ok to libxml
   (string-match "<div\s?id=\"tags\"\s?" raw-content)))

(defun leet-code--display-info (qname)
  (interactive
   (list
    (completing-read
     "choose question: "
     leet-code-quests
     nil
     t)))
  (let* ((data
          (request-response-data
           (request
            (concat leet-code-base-url "/problems/" qname "/")
            :parser 'buffer-string
            :headers leet-code-request-header
            :sync t
            )
           ))
         (info (leet-code--parse-info data))
         (start (point-min))
         (end (length info))
        )
    (save-excursion
      (goto-char start)
      (insert info)
      (newline)
      (shr-render-region start end)
      (comment-region start (point))
      (fill-region start (point))
      (when (y-or-n-p "insert code template? ")
        (insert (leet-code--parse-code data))
        (newline)
          ))
    ))


(provide 'leet-code)

;;; leet-code.el ends here
