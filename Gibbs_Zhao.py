import numpy as np
import matplotlib.pyplot as plt

def inverse_sampler(B,theta):
    u = np.random.random()
    norm_constant = 1-np.exp(-B*theta)
    x = -np.log(1-u*norm_constant)/theta
    return(x)


def Gibbs_sampler(B,sample_size):
    sample_chain = np.zeros(sample_size*2).reshape(2,sample_size)
    start_y = B/2
    for i in range(sample_size):
        sample_chain[0,i] = inverse_sampler(B,start_y)
        sample_chain[1,i] = inverse_sampler(B,sample_chain[0,i])
        start_y = sample_chain[1,i]
    return(sample_chain)

B = 5
T = 5000
chain = Gibbs_sampler(B,T)
plt.hist(chain[1,:],bins=50)
plt.show()
