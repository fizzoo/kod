#! /usr/bin/env python3

# Program to visualize parameter space of linear fitting

# By:
# [fizzo](https://github.com/fizzoo)
# [Konrad Magnusson](https://github.com/konradmagnusson)

import numpy as np
import matplotlib.pyplot as plt
import functools
import seaborn as sns
sns.set()


def our_random(x):
    """Get a sample from the "true" model at x."""
    epsilon = np.random.randn()/10
    k = 0.6
    m = 1.4
    return k*x + m + epsilon


def gauss(mean, sigma):
    """Create a gaussian distribution, returning it as a closure."""
    meannp = np.array(mean)
    sigmanp = np.array(sigma)

    const = 1/np.sqrt(np.linalg.det(2*np.pi*sigmanp))
    inv = np.linalg.inv(sigmanp)

    def f(x):
        diff = np.array(x-meannp)
        return const*np.exp(-1/2*(diff.T @ inv @ diff))
    return f


def prob_for(x, y, w):
    """Calculate likelihood 'P(y | w, x)' (not log-likelihood)"""
    guess_y = np.array([w[0] + w[1] * _x for _x in x])
    diff = y - guess_y
    err_to_prob = gauss([0], [[1]])
    probs = [err_to_prob(x) for x in diff]
    prodprobs = functools.reduce(lambda x, y: x*y, probs, 1)
    return prodprobs


def color(x):
    """Return a nice color for a line depending on our confidence x ([0, 1])"""
    r = int(255*x)
    g = int(128-np.abs(128-256*x))
    b = int(255-255*x)

    r -= g//2
    b -= g//2

    str = "#%0.2X%0.2X%0.2X" % (r, g, b)
    return str


def plot_transpar(w, alpha):
    """Plot y=kx+m with w containing [m k]"""
    x = np.linspace(-1.5, 1.5, 3)
    y = [w[0] + w[1]*_x for _x in x]
    plt.plot(x, y, alpha=alpha, color=color(alpha), zorder=1)


def main():
    # Space in image for w0, w1
    lin = np.linspace(-1.0, 3.0, 50)
    wspace = np.array([np.array([i, j]) for i in lin for j in lin])

    # "Sampled" data, vary the number here for accuracy
    data_x = np.linspace(-1.5, 1.5, 20)
    data_y = [our_random(i) for i in data_x]
    w = np.array([prob_for(data_x, data_y, w) for w in wspace])

    prior = gauss([0, 0], [[1, 0], [0, 1]])
    p = np.array([prior(w) for w in wspace])

    posterior = w*p

    maxpost = posterior[posterior.argsort()][-1]

    # For subplotting
    fig = plt.figure()

    # The prior 2d space
    ax = fig.add_subplot(131)
    plt.title("Prior")
    X, Y = np.meshgrid(lin, lin)
    prior2d = [prior([i, j]) for i in lin for j in lin]
    prior2d = np.reshape(prior2d, (len(lin), len(lin)))
    ax.contourf(X, Y, prior2d, 100, cmap=plt.cm.jet)

    # The posterior 2d space
    ax = fig.add_subplot(132)
    plt.title("Posterior")
    X, Y = np.meshgrid(lin, lin)
    posterior2d = np.reshape(posterior, (len(lin), len(lin)))
    ax.contourf(X, Y, posterior2d, 100, cmap=plt.cm.jet)

    # Visualization of best models
    ax = fig.add_subplot(133)
    plt.title("Best models")
    for k, v in enumerate(posterior):
        plot_transpar(wspace[k], v/maxpost)
    plt.scatter(data_x, data_y, zorder=2)

    # Draw it all
    plt.show()


if __name__ == "__main__":
    main()
