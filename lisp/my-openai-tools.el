(defun get-openai-api-key ()
  "Retrieve OpenAI API key from .authinfo."
  (let ((auth-info (nth 0 (auth-source-search :host "api.openai.com"
                                              :user "apikey"
                                              :require '(:user :secret)))))
    (if auth-info
        (let ((secret (plist-get auth-info :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "No OpenAI API key found in .authinfo"))))

(provide 'my-openai-tools)
