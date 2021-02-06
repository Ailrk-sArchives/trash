"""
Hand made average perceptron.
It only make binary classification.
The average penalty make it adapts for even extremely
deistributed data.
"""
import numpy as np


class Perceptron:
    def __init__(self, X: np.ndarray, Y: np.ndarray, maxiter: int):
        """
        X: training data
        Y: training label
        make sure Y in {1, -1}
        """
        self.X = X  # Train data
        self.Y = Y  # Train label
        self.max_iter: int = maxiter
        self.activation: float = 0.0
        self.w: np.ndarray = np.zeros(X.shape[1])
        self.b: float = 0

        # penalty for overfitting.
        self.mu: np.ndarray = np.zeros(X.shape[1])
        self.beta: float = 0
        self.count = 1

    def train(self) -> None:
        # make sure shapes of matrices are correct.
        assert self.X.shape[0] == self.Y.shape[0] and \
                self.X.shape[1] == self.w.shape[0] and \
                self.max_iter < len(self.X)

        for i in range(0, self.max_iter):
            for x, y in list(zip(self.X, self.Y)):
                self.activation = np.dot(self.w, x) + self.b
                if self.activation * y <= 0:
                    self.w = self. w + y * x  # reweight.
                    self.b = self.b + y

                    self.mu = self.mu + y * self.count * x
                    self.beta = self.beta + y * self.count
                self.count += 1

        # get avg weight and bias.avoid overfitting.
        self.w = self.w - (1 / self.count) * self.mu
        self.b = self.b - (1 / self.count) * self.beta

    def predict(self, x_test: np.ndarray) -> np.ndarray:
        """
        x: test data, a MxN matrix.
        """
        predict_list: np.ndarray = []

        for xh in x_test:
            predict_list.append(self.__predict_one(xh))
        return predict_list

    def accuracy(self, predicts: np.ndarray, y_test: np.ndarray) -> None:
        """
        Show the predict result.
        """
        accuracy: float = 0.0
        print("============ Comparing ===============")
        for pr, t in list(zip(predicts, y_test)):
            if pr == t:
                accuracy += 1
                print("bingo", pr, t)
            else:
                print("miss", pr, t)
        print("======================================")
        accuracy = accuracy / len(predicts)
        print("accuracy of hand made perceptron is {}".format(accuracy))

    def __predict_one(self, x: np.ndarray) -> int:
        self.activation = np.dot(self.w, x) + self.b
        return int(np.sign(self.activation))
