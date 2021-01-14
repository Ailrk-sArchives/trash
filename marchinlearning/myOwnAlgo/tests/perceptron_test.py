import sys
sys.path.append("../")
import numpy as np
from sklearn import datasets
from soylearn import perceptron


# preamble, change all 0 in data to -1
breast_cancer = datasets.load_breast_cancer()
bc_data = breast_cancer.data
bc_target = np.array([-1 if b == 0 else 1 for b in breast_cancer.target])

accuracy = 0.0
predicts: np.ndarray = []
X_train = bc_data[:400]
Y_train = bc_target[:400]

X_test = bc_data[400:500]
Y_test = bc_target[400:500]

p = perceptron.Perceptron(X_train, Y_train, maxiter=350)
p.train()

predicts = p.predict(X_test)
p.accuracy(predicts, Y_test)

