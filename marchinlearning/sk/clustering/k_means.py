import numpy as np
import matplotlib.pyplot as plt

from sklearn.cluster import KMeans
from sklearn.datasets import make_blobs

plt.figure(figsize=(12,12))

# make samples.
n_samples = 1000
random_state = 200
X, y = make_blobs(n_samples=n_samples, random_state=random_state)
# note

# Incorrect # of clusters.
y_pred = KMeans(n_clusters=2, random_state=random_state).fit_predict(X)

plt.subplot(221)
plt.scatter(X[:, 0], X[:, 1], c=y_pred)
plt.title("Incorrect # of Blobs")

# Anisotropicly distributed data
transformation = [[0.60834549, -0.63667341],
                  [-0.40887718, 0.85253229]]
X_ansio = np.dot(X, transformation)
y_pred = KMeans(n_clusters=3, random_state=random_state).fit_predict((X_ansio))

plt.subplot(222)
plt.scatter(X_ansio[:, 0], X_ansio[:, 1], c=y_pred)
plt.title("Anisotropicly Distributed Bolbs")

# Different variance
X_varied, y_varied = make_blobs(n_samples=n_samples,
                                cluster_std=[1.0, 2.5, 0.1],
                                random_state=random_state)
y_pred = KMeans(n_clusters=3, random_state=random_state).fit_predict(X_varied)

plt.subplot(223)
plt.scatter(X_varied[:, 0], X_varied[:, 1], c=y_varied)
plt.title("Unequal Variance")

# unevenly sized bolbs
X_filtered = np.vstack(
        (X[y == 0][:500], X[y == 1][:100], X[y == 2][:10]))
y_pred = KMeans(n_clusters=3,
                random_state=random_state).fit_predict(X_filtered)

plt.subplot(224)
plt.scatter(X_filtered[:, 0], X_filtered[:, 1], c=y_pred)
plt.title("Unevenly Sized Blobs")

plt.show()

