(require data-science math plot)

(define df (read-csv (expand-user-path
		      "~/data/fake_news_summer_scholars_17/snopes/fake_word_freq.csv")))
(define dr (read-csv (expand-user-path
		      "~/data/fake_news_summer_scholars_17/snopes/real_word_freq.csv")))

(define tmpf (map (λ (x) `(,(first x) ,(string->number (second x)))) (cdr df)))
(define tmpr (map (λ (x) `(,(first x) ,(string->number (second x)))) (cdr dr)))
(define df tmpf)
(define dr tmpr)
;;; Remove the word "like" from both data sets. It probably shouldn't
;;; be counted in our sentiment scores due to its use as a simile.
(define df
  (filter (λ (x) (not (string=? "like" (first x)))) df))
(define dr
  (filter (λ (x) (not (string=? "like" (first x)))) dr))

(define fake-sentiment (list->sentiment df #:lexicon 'nrc))
(define real-sentiment (list->sentiment dr #:lexicon 'nrc))

;;; Fake news plot
(let ([counts (aggregate sum ($ fake-sentiment 'sentiment) ($ fake-sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot-file (list
		(tick-grid)
		(discrete-histogram
		 (sort counts (λ (x y) (> (second x) (second y))))
		 #:color "MediumSlateBlue"
		 #:line-color "MediumSlateBlue"))
	       "fake_news_affective_counts.pdf"
	       #:x-label "Affective Label"
	       #:y-label "Frequency")))

;;; Real news plot
(let ([counts (aggregate sum ($ real-sentiment 'sentiment) ($ real-sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot-file (list
		(tick-grid)
		(discrete-histogram
		 (sort counts (λ (x y) (> (second x) (second y))))
		 #:color "MediumSlateBlue"
		 #:line-color "MediumSlateBlue"))
	       "real_news_affective_counts.pdf"
	       #:x-label "Affective Label"
	       #:y-label "Frequency")))


;;; Or, use the bing lexicon to determine the ratio of
;;; positive-to-negative words 
(define fake-sentiment (list->sentiment df #:lexicon 'bing))
(define real-sentiment (list->sentiment dr #:lexicon 'bing))

;;; Fake news plots
(parameterize ([plot-height 200])
  (plot-file (discrete-histogram
	      (aggregate sum ($ fake-sentiment 'sentiment) ($ fake-sentiment 'freq))
	      #:y-min 0
	      #:invert? #t
	      #:color "MediumOrchid"
	      #:line-color "MediumOrchid")
	     "fake_sentiment_polarity.pdf"
	     #:x-label "Frequency"
	     #:y-label "Sentiment Polarity"))

;;; Is the difference in sentiment polarity statistically significant?
(let ([counts (aggregate sum ($ fake-sentiment 'sentiment) ($ fake-sentiment 'freq))])
  (chi-square-goodness counts '(.5 .5)))

;; '#hash(('criterion . 3.8414588206941254)
;;        ('alpha . 0.05)
;;        ('df . 1)
;;        ('chisqr . 1271.4956305089083)
;;        ('result . "significant"))

;;; Real news plots
(parameterize ([plot-height 200])
  (plot-file (discrete-histogram
	      (reverse (aggregate sum ($ real-sentiment 'sentiment) ($ real-sentiment 'freq)))
	      #:y-min 0
	      #:invert? #t
	      #:color "MediumOrchid"
	      #:line-color "MediumOrchid")
	     "real_sentiment_polarity.pdf"
	     #:x-label "Frequency"
	     #:y-label "Sentiment Polarity"))

;;; Is the difference in sentiment polarity statistically significant?
(let ([counts (aggregate sum ($ real-sentiment 'sentiment) ($ real-sentiment 'freq))])
  (chi-square-goodness counts '(.5 .5)))

;; '#hash(('criterion . 3.8414588206941254)
;;        ('alpha . 0.05)
;;        ('df . 1)
;;        ('chisqr . 2.9669472239280426)
;;        ('result . "not-significant"))

;;; We can also look at which words are contributing the most to our
;;; positive and negative sentiment scores. We'll look at the top 15
;;; influential (i.e., most frequent) positive and negative words
(define negative-tokens
  (take (cdr (subset fake-sentiment 'sentiment "negative")) 15))
(define positive-tokens
  (take (cdr (subset fake-sentiment 'sentiment "positive")) 15))

;;; Some clever reshaping for plotting purposes
(define n (sort (map (λ (x) (list (first x) (- 0 (third x))))
		     negative-tokens)
		(λ (x y) (< (second x) (second y)))))
(define p (sort (map (λ (x) (list (first x) (third x)))
		     positive-tokens)
		(λ (x y) (< (second x) (second y)))))

;;; Plot the results
(parameterize ((plot-width 800)
	       (plot-x-tick-label-anchor 'right)
	       (plot-x-tick-label-angle 90))
  (plot-file (list
	      (tick-grid)
	      (discrete-histogram n
				  #:y-min -600
				  #:y-max 1250
				  #:color "OrangeRed"
				  #:line-color "OrangeRed"
				  #:label "Negative Sentiment") 
	      (discrete-histogram p
				  #:y-min -600
				  #:y-max 1250
				  #:x-min 15
				  #:color "LightSeaGreen"
				  #:line-color "LightSeaGreen"
				  #:label "Positive Sentiment"))
	     "fake_sentiment_word_contribution.pdf"
	     #:x-label "Word"
	     #:y-label "Contribution to sentiment"))

;;; Same, but for real news articles
(define negative-tokens
  (take (cdr (subset real-sentiment 'sentiment "negative")) 15))
(define positive-tokens
  (take (cdr (subset real-sentiment 'sentiment "positive")) 15))

;;; Some clever reshaping for plotting purposes
(define n (sort (map (λ (x) (list (first x) (- 0 (third x))))
		     negative-tokens)
		(λ (x y) (< (second x) (second y)))))
(define p (sort (map (λ (x) (list (first x) (third x)))
		     positive-tokens)
		(λ (x y) (< (second x) (second y)))))

;;; Plot the results
(parameterize ((plot-width 800)
	       (plot-x-tick-label-anchor 'right)
	       (plot-x-tick-label-angle 90))
  (plot-file (list
	      (tick-grid)
	      (discrete-histogram n
				  #:y-min -600
				  #:y-max 1500
				  #:color "OrangeRed"
				  #:line-color "OrangeRed"
				  #:label "Negative Sentiment") 
	      (discrete-histogram p
				  #:y-min -600
				  #:y-max 1500
				  #:x-min 15
				  #:color "LightSeaGreen"
				  #:line-color "LightSeaGreen"
				  #:label "Positive Sentiment"))
	     "real_sentiment_word_contribution.pdf"
	     #:x-label "Word"
	     #:y-label "Contribution to sentiment"))

