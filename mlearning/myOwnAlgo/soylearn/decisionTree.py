"""
Hand made Classification and Regression Tree.
It has two cost function with respect to classification
problems and Regression problems.
"""
import numpy as np
from .baseline_algorithms import zero_rule


class DecisionTree:
    """
    a simple CART
    """
    def __init__(self, X: np.ndarray, Y: np.ndarray, maxdepth: int,
                 minnodes: int):
        self.X: np.ndarray = X
        self.Y: np.ndarray = Y
        self.maxdepth = maxdepth
        self.minnodes = minnodes
        self.labels: list = list(set(Y))

    def train(self) -> None:
        pass

    def predict(self) -> np.ndarray:
        pass

    def __split(self):
        """ split accrodig to the smallest gini index """
        pass

    def __split_one(self):
        """ split accroding to one attribute """
        pass

    def gini_index(self, groups_x: np.ndarray, groups_y: np.ndarray) -> float:
        """
        calculate the gini index
        given a split dataset, calculate the gini for this specific splition

        groups_x: splited featrues
        groups_y: splited labels
        """
        # count number of samples
        total_samples: float = float(sum([len(gx) for gx in groups_x]))
        labels_set = set(self.Y)  # the category of labels
        gini: float = 0.0
        for gx, gy in list(zip(groups_x, groups_y)):
            size = float(len(gx))  # the size of sample in one group
            if size == 0:
                continue

            score: float = 0.0
            for label_val in labels_set:
                propotion = list(gy).count(label_val) / size
                score += propotion ** 2
            gini += (1.0 - score) * (size / total_samples)
        return gini

    def __terminal(self):
        """ create terminal node """
        pass

    def build_tree(self):
        pass
