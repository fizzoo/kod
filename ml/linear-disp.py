#! /usr/bin/env python3

# Program to visualize parameter space of linear fitting

# By:
# [fizzo](https://github.com/fizzoo)
# [Konrad Magnusson](https://github.com/konradmagnusson)

import numpy as np
import matplotlib.pyplot as plt
import functools
from mpl_toolkits.mplot3d import Axes3D # 'projection=3d'
import seaborn as sns
sns.set()


# The true model we're trying to find, with some noise
def our_random(x):
    epsilon = np.random.randn()/30
    k = 0.6
    m = 1.4
    return k*x + m + epsilon


# Gaussian
def gauss(mean, sigma):
    meannp = np.array(mean)
    sigmanp = np.array(sigma)

    const = 1/np.sqrt(np.linalg.det(2*np.pi*sigmanp))
    inv = np.linalg.inv(sigmanp)

    def f(x):
        diff = np.array(x-meannp)
        return const*np.exp(-1/2*(diff.T @ inv @ diff))
    return f


# Calculate likelihood 'P(y | w)' (not log-likelihood)
def prob_for(x, y, w):
    guess_y = np.array([w[0] + w[1] * _x for _x in x])
    diff = y - guess_y
    err_to_prob = gauss([0], [[1]])
    probs = [err_to_prob(x) for x in diff]
    prodprobs = functools.reduce(lambda x, y: x*y, probs, 1)
    return prodprobs


# Plot y=kx+m with w containing [m k]
def plot_transpar(w, alpha):
    x = np.linspace(-1.5, 1.5, 3)
    y = [w[0] + w[1]*_x for _x in x]
    plt.plot(x, y, alpha=alpha, color="r")


def main():
    # Space in image for w0, w1
    lin = np.linspace(-1.0, 3.0, 20)
    wspace = np.array([np.array([i, j]) for i in lin for j in lin])

    data_x = np.linspace(-1.5, 1.5, 20)
    data_y = [our_random(i) for i in data_x]
    w = np.array([prob_for(data_x, data_y, w) for w in wspace])

    maxw = w[w.argsort()][-1]

    fig = plt.figure()
    ax = fig.add_subplot(122)
    for k, v in enumerate(w):
        plot_transpar(wspace[k], v/maxw)

    ax = fig.add_subplot(121, projection="3d")
    wspace_x = [a[0] for a in wspace]
    wspace_y = [a[1] for a in wspace]
    ax.scatter(wspace_x, wspace_y, w)
    plt.show()


if __name__ == "__main__":
    main()
