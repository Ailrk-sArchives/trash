import numpy as np
import matplotlib.pyplot as plt

from sklearn.cluster import KMeans
from sklearn.datasets import make_blobs


def subplot(pos: int, x, y, c, title: str):
    plt.subplot(pos)
    plt.scatter(x, y, c=c)
    plt.title(title)


plt.figure(figsize=(12, 12))

n_samples = 1500
random_state = 170
X, y = make_blobs(n_samples=n_samples, random_state=random_state)

# incorrect number of clusters
y_pred = KMeans(n_clusters=2, random_state=random_state).fit_predict(X)

subplot(221, X[:, 0], X[:, 1], c=y_pred, title="Incorrect number of blobs")

# Anisotropicly distributed data
transformation = [[0.60834549, -0.63667341], [-0.40887718, 0.85253229]]
X_aniso = np.dot(X, transformation)
y_pred = KMeans(n_clusters=3, random_state=random_state).fit_predict(X_aniso)

subplot(222, X_aniso[:, 0], X_aniso[:, 1],
        c=y_pred, title="Anisotropicly distributed blobs")

# Different variance
X_varied, y_varied = make_blobs(n_samples=n_samples,
                                cluster_std=[1.0, 2.5, 0.5],
                                random_state=random_state)
y_pred = KMeans(n_clusters=3, random_state=random_state).fit_predict(X_varied)

subplot(223, X_varied[:, 0], X_varied[:, 1],
        c=y_pred, title="Unequal variance")


# Unevenly sized blobs
X_filtered = np.vstack((X[y == 0][:500], X[y == 1][:100], X[y == 2][:10]))
y_pred = KMeans(
    n_clusters=3, random_state=random_state).fit_predict(X_filtered)

subplot(224, X_filtered[:, 0], X_filtered[:, 1],
        c=y_pred, title="Unevenly Sized Blobs")

plt.show()

# Take away:
# - K means might produce unintuitive and unexpected clusters.
