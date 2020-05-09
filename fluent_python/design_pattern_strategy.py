"""
Strategy pattern:
    define a family of algorithms, encapsulate each one, and make
    them interchangeable.
    (Context)           (strategy)
    |Order|----------> |Promotion|
                           |
                   .---------------.
                   |       |       |
                Promo1   Promo2   Promo3
            (concret strategies)

    even though it is context choose strategy, method in strategy still needs
    to get attributes in context to do computation. So when a data need to be
    applied by a method it always need to pass itself to that method.
    like discount(self)

    concret strategies suppose to be state free, so bascially they are functions.
"""
from abc import ABC, abstractmethod
from collections import namedtuple


Customer = namedtuple('Customer', 'name fidelity')


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


class Promotion(ABC):  # the Strategy: an Abstract Base Class

    @abstractmethod
    def discount(self, order):
        """Return discount as positive dollar"""


class FidelityPromo(Promotion):
    """5% discount for customers with 1000 or more fidility points"""

    def discount(self, order):
        return order.total() * .05 if order.customer.fidelity >= 1000 else 0


class BulkItemPromo(Promotion):
    """10% discount for each lineitem with 20 or more units"""

    def discount(self, order):
        discount = 0
        for item in order.cart:
            if item.quantity >= 20:
                discount += item.total() * .1
        return discount


class LargeOrderPromo(Promotion):
    """7% discount for order with 10 or more disctinct items"""

    def discount(self, order):
        distinct_items = {item.product for item in order.cart}  # distinct.
        if len(distinct_items) >= 10:
            return order.total() * .07
        return 0


def fidelity_promo(order):
    """ higher order function to replace concret strategy"""
    return order.total() * .05 if order.customer.fidelity >= 1000 else 0


def bulk_item_promo(order):
    discount = 0
    for item in order.cart:
        if item.quantity >= 20:
            discount += item.total() * .1
    return discount


def large_order_promo(order):
    distinct_items = {item.product for item in order.cart}  # distinct.
    if len(distinct_items) >= 10:
        return order.total() * .07
    return 0


def best_promo(order):
    """select the promo strategy with the highest discount"""
    return max(promo(order) for promo in [fidelity_promo,
                                          bulk_item_promo,
                                          large_order_promo])


def hack_best_promo(order):
    promos = [globals()[name] for name in globals()
              if 'best' not in name
              and name.endswith('_promo')]
    return max(promo(order) for promo in promos)


if __name__ == "__main__":
    joe = Customer('John Doe', 0)
    ann = Customer('Ann Smith', 1100)
    cart = [LineItem('banana', 4, .5),
            LineItem('apple', 10, 1.5),
            LineItem('watermellon', 5, 5.0)]
    banana_cart = [LineItem('banana', 30, .5)]
    long_cart = [LineItem(str(i), 1, 1.0) for i in range(20)]
    print('Fidelity Promo with class strategy')
    print(Order(joe, cart, FidelityPromo()))
    print(Order(ann, cart, FidelityPromo()))

    # use higher order function to replace class
    print("use higher order function instead of class for fidelity promo")
    print(Order(ann, cart, fidelity_promo))

    print("Bulk strategy and large order strategy")
    print(Order(ann, banana_cart, BulkItemPromo()))
    print(Order(ann, long_cart, LargeOrderPromo()))

    fp = FidelityPromo()
    print("Create a singleton concret strategy to avoid flyweight")
    print(Order(ann, cart, fp))
    print(Order(joe, cart, fp))

    print("find the best strategy with higher order funcs")
    print(Order(ann, cart, best_promo))
    print(Order(ann, cart, hack_best_promo))
