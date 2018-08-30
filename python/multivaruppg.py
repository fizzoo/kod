import numpy as np
from scipy.stats import norm

print()
B = np.array([[1, 0], [3, 1]])
MUx = np.array([1, 1])
Cx = np.array([[3, 1], [1, 2]])

Cuv = B @ Cx @ B.T
MUuv = B @ MUx
print("Cuv", Cuv)
print("MUuv", MUuv)

Ucond = 3

MUcond = MUuv[0] + Cuv[0, 1] * (1.0/Cuv[1, 1]) * (Ucond - MUuv[1])
Ccond = Cuv[0, 0] - Cuv[0, 1] * (1.0/Cuv[1, 1]) * Cuv[1, 0]
print("MUcond", MUcond)
print("Ccond", Ccond)

prob = 1.0 - norm.cdf(2, loc=MUcond, scale=np.sqrt(Ccond))
print("prob", prob)
analytical = 1.0 - norm.cdf(9/np.sqrt(7))
print("analytical", analytical)
print("diff = ", prob - analytical)
