import sys
sys.path.append("../")
from soylearn.decisionTree import DecisionTree
from sklearn.datasets import load_breast_cancer

bc = load_breast_cancer()

t = DecisionTree(bc.data, bc.target, 0, 0)

gini = t.gini_index([bc.data[:50], bc.data[50:100]], [bc.target[:50], bc.target[50:]])
print(gini)
