import sys
sys.path.append("../")
from sklearn import datasets
from soylearn import baseline_algorithms as baseline_algo

bc = datasets.load_breast_cancer()
boston = datasets.load_boston()

random_pre_clf = baseline_algo.random_prediction(
        bc.target[:200], bc.target[200:])
random_pre_reg = baseline_algo.random_prediction(
        boston.target[:200], boston.target[200:])

z_r_clf = baseline_algo.zero_rule(
        bc.target[:200], bc.target[200:], mode="clf")
z_r_reg = baseline_algo.zero_rule(
        boston.target[:200], boston.target[200:], mode="reg")

print("random prediction clf: ", random_pre_clf, "\n===============")
print("random prediction reg: ", random_pre_reg, "\n===============")
print("zero rule clf: ", z_r_clf, "\n===============")
print("zero rule reg: ", z_r_reg, "\n===============")
