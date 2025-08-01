import numpy as np
#import matplotlib.pyplot as plt
from statsmodels.tsa.arima_model import ARMA
import random
import pandas as pd



thetas = [0.1, 0.2, 0.5, 0.05]
# thetas = [0.1, 0.2, 0.5, 0.05, 0.00001]
phis = [0.1, 0.2, 0.3]

def generate_wn(n, sigma=1):
    """
    Generate white noise with a conditional mean of zero
    """

    return np.random.normal(0, sigma, size=n)



def generate_arma(n, phis, thetas, mu, sigma=1):
    p = len(phis)
    q = len(thetas)

    adj_n = n + max(p, q)      # use max to make sure we cover the lack of coefficients.
    e_series = generate_wn(adj_n)

    arma = [e_series[0]]  # start the series with a random value (same as AR)

    for i in range(1, adj_n):
        visible_phis = phis[0:min(p, i)]
        visible_thetas = thetas[0:min(q,i)]

        reversed_phis = visible_phis[::-1]
        reversed_thetas = visible_thetas[::-1]

        visible_series = arma[i - min(p, i):i]
        visible_e_series = e_series[i - min(q, i):i]

        try:     # getting e_t if we can
            e_t = visible_e_series[-1]
        except IndexError:
            e_t = 0

        # main equation
        ar_t = + np.dot(reversed_phis, visible_series)
        ma_t = mu + e_t + np.dot(reversed_thetas, visible_e_series)
        arma_t = ar_t + ma_t
        arma.append(arma_t)

    arma = arma[max(p, q):]   # dr opping the first values did not use all phis or thetas


    return arma


# grab a random integer to scale the data set

def randomInt():
    """
    Create a random scaler value to use as an input for generate_arma
    """
    random_integer = random.randint(5, 15)

    return random_integer



my_dict = {}

for i in range(0, 7):

    INT = randomInt()
    res = generate_arma(730, phis, thetas, INT)

    #print(res)

    # save output in our dict

    my_dict[i] = res


#print(my_dict)

df = pd.DataFrame(my_dict)

# print(df)

df.rename(columns={0: "media_1_spend",
                   1:"media_1_traffic",
                   2: "media_2_spend",
                   3: "media_2_traffic",
                   4: "media_3_spend",
                   5: "media_3_traffic",
                   6: "sales"}, inplace=True)


#print(df)
# print(df.columns.tolist())


df.to_csv("simulated_data.csv", index=False)

# for the 7 coefficients we need to generate data and save it as a dictionary of list
# note each observation is a day



