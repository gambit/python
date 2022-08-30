(import (_six python))
(import (github.com/udem-dlteam python))
(python-exec "import matplotlib.pyplot as plt")
\import numpy as np

(define N 5)
(define men-means (vector 20 35 30 35 -27))
(define women-means (vector 25 32 34 20 -25))
(define men-std (vector 2 3 4 1 2))
(define women-std (vector 3 5 2 3 3))
(define ind \np.arange(`N))
(define width 0.35)

(python-exec "fig, ax = plt.subplots()")

\p1=ax.bar(`ind, `men-means, `width, yerr=`men-std, label='Men')
\p2=ax.bar(`ind, `women-means, `width, bottom=`men-means,
                 yerr=`women-std, label='Women')

\ax.axhline(0, color='grey', linewidth=0.8)
\ax.set_ylabel('Scores')
\ax.set_title('Scores by group and gender')
\ax.set_xticks(`ind, labels=['G1', 'G2', 'G3', 'G4', 'G5'])
\ax.legend()

\ax.bar_label(p1, label_type='center')
\ax.bar_label(p2, label_type='center')
\ax.bar_label(p2)

\plt.show()
