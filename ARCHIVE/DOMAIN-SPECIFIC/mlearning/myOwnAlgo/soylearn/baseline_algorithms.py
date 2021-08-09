"""
Baseline algorithm. Contains random prediction and zero rule alorithm
For classification now.

2019-05-16
"""
from typing import Union
import numpy as np
from random import seed, randrange


def random_prediction(labels: np.ndarray,
                      test_labels: np.ndarray) -> np.ndarray:
    """
    dumb ass
    """
    seed(1)
    unique: list = list(set(labels))

    test_len: int = len(test_labels)
    predicts: np.ndarray = np.zeros(test_len)  # fixed size array.

    for i, row in enumerate(test_labels):
        rand_index = randrange(len(unique))
        predicts[i] = unique[rand_index]
    return predicts


def zero_rule(labels: np.ndarray,
              test_labels: np.ndarray, mode="clf") -> np.ndarray:
    """
    mode == 'classification': classification,
         == 'regression': regression.
    zero_rule choose the most frequent label in labels.
    """
    seed(1)
    labels = list(labels)
    prediction: Union[int, float] = 0

    test_len: int = len(test_labels)
    predicts: np.ndarray = np.zeros(test_len)

    if mode == "reg":
        prediction = sum(labels) / float(len(labels))
    elif mode == "clf":  # default prediction.
        prediction = max(set(labels), key=labels.count)

    for i in range(test_len):
        predicts[i] = prediction
    return predicts
