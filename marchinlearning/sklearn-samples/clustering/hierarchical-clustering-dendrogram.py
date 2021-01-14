# Plot dendrogram of a hierarchical clustering
# with AgglomerativeClustering

import numpy as np
from matplotlib import pyplot as plt
from scipy.cluster.hierarchy import dendrogram
from sklearn.datasets import load_iris
from sklearn.cluster import AgglomerativeClustering


def plot_dendrogram(model: AgglomerativeClustering, **kwargs):
    counts = np.zeros(model.children_.shape[0])
    n_samples: int = len(model.labels_)
    for i, merge in enumerate(model.children_):
        current_count = 0
        for child_idx in merge:
            if child_idx < n_samples:
                current_count += 1
            else:
                current_count += counts[child_idx - n_samples]
        counts[i] = current_count
    linkage_matrix = np.column_stack([model.children_,
                                      model.distances_,
                                      counts]).astype(float)
    dendrogram(linkage_matrix, **kwargs)


if __name__ == "__main__":
    iris = load_iris()
    X = iris.data

    # setting distance_threshold=0 to ensures we compute the entire full tree.
    model = AgglomerativeClustering(distance_threshold=0, n_clusters=None)
    model = model.fit(X)

    plt.title("Hierachical Clustering Dendrogram")
    plot_dendrogram(model, truncate_mode='level', p=3)
    plt.xlabel("NUmber of points in node (index of point if no parenthesis).")
    plt.show()
