from collections import namedtuple
from typing import List


promos: List = []
Customer = namedtuple('Customer', 'name fidelity')


def promotion(promo_func):
    promos.append(promo_func)
    return promo_func


class LineItem:

    def __init__(self, product, quantity, price):
        self.product = product
        self.quantity = quantity
        self.price = price

    def total(self):
        return self.price * self.quantity


class Order:  # context

    def __init__(self, customer, cart, promotion=None):
        self.customer = customer
        self.cart = list(cart)
        self.promotion = promotion

    def total(self):
        if not hasattr(self, '_total'):
            self._total = sum(item.total() for item in self.cart)
        return self._total

    def due(self):
        if self.promotion is None:
            discount = 0
        elif callable(self.promotion):
            discount = self.promotion(self)
        else:
            discount = self.promotion.discount(self)
        return self.total() - discount

    def __repr__(self):
        fmt = '<Order total: {:.2f} due: {:.2f}>'
        return fmt.format(self.total(), self.due())


@promotion
def fidelity(order):
    """ higher order function to replace concret strategy"""
    return order.total() * .05 if order.customer.fidelity >= 1000 else 0


@promotion
def bulk_item(order):
    discount = 0
    for item in order.cart:
        if item.quantity >= 20:
            discount += item.total() * .1
    return discount


@promotion
def large_order(order):
    distinct_items = {item.product for item in order.cart}  # distinct.
    if len(distinct_items) >= 10:
        return order.total() * .07
    return 0


def best_promo(order):
    """get the best promo"""
    return max(promo(order) for promo in promos)


if __name__ == "__main__":
    joe = Customer('John Doe', 0)
    ann = Customer('Ann Smith', 1100)
    cart = [LineItem('banana', 4, .5),
            LineItem('apple', 10, 1.5),
            LineItem('watermellon', 5, 5.0)]
    banana_cart = [LineItem('banana', 30, .5)]
    long_cart = [LineItem(str(i), 1, 1.0) for i in range(20)]
    print(Order(joe, banana_cart, best_promo))



