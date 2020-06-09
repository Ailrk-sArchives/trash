"""
be familiar with sqlalchemy.

ORM===================================
              ORM
core =================================
  | ddl |, | sql expr | , | engine |
    | dialect |, | connection |
dbapi ================================
          python DBAPI
======================================

ACID Transaction.
    atomic,
    consistency (order),
    isolation (changes inside transaction aren't visible),
        (historically implemented bylock,
         modern databases use `multiversion councurrency control`)
    durability (commit guarantee).
"""

from sqlalchemy import Table, Column, Enum, Numeric
from sqlalchemy import Integer, String
from sqlalchemy import Unicode, UnicodeText
from sqlalchemy import MetaData
from sqlalchemy import DateTime
from sqlalchemy import ForeignKey
from sqlalchemy import ForeignKeyConstraint
from sqlalchemy import create_engine
from sqlalchemy import inspect
from sqlalchemy import and_, or_, select, func
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import Session
from sqlalchemy.orm import aliased
from sqlalchemy import func
from sqlalchemy import relationship


Base = declarative_base()

# can have as many Base as you want.
Base1 = declarative_base()


"""
NOTE PEP249
"""


"""
Engine Facts:
    Executing via Engine directly is called connectionless exection.
    Engine connect and disconect for us.

    Using Connection is explict execution. we control thhe span of
    a connection in use.

    Engine ususally use a connection pool, which means "disconecting"
    often means the connection just return to the pool.

    SQL we send to engine.execute() as a string is not modified,
    is consumed by the DBAPI verbatim.
"""


def engine_fn1():
    """
    By default their will be a connection pool run on the background
    engine.execute(...) will be automatically assigned with a connection
    """
    # Engine
    engine = create_engine("sqlite:///some.db")

    # different dbapi has different url
    # engine2 = create_engine("postgresql:///some:db@localhost/test")

    # Result. (like a handle on returned value on cursor.)
    # NOTE: DBAPI doesn't autocommit by default.
    # execute() will "autocommit" statements like INSERT by default.
    result = engine.execute(
        "select emp_id, emp_name from employee where emp_id=:emp_id",
        emp_id=3)

    # fetch one row.
    row = result.fetchone()
    print(row)
    print(row['emp_name'])
    print(row['emp_id'])

    # result is iterartor.
    for row in result:
        print(row)

    # when get all the rows under the cursor
    # the result will close automatically.
    # clean up the connection back to the connection pool.
    result.close()


def engine_fn2():
    """
    control the scope of the connection by explicitly create a conection
    from engine, and execute sql under the same connection.
    """
    engine = create_engine("sqlite:///some.db")
    conn = engine.connect()
    result = conn.execute("select * from employee")
    result.fetchall()
    conn.close()


def engine_fn3():
    """
    run several statements inside a transaction.
    connection features a begin() method that returns a Transaction.

    `Transaction` is a session that multiple sql are executed within
    one connection. it happens on one connection at a time.

    In the senarios the connection is a session to the database.
    """
    engine = create_engine("sqlite:///some.db")

    conn = engine.connect()

    trans = conn.begin()
    conn.execute("insert into employee (emp_name) values (:emp_name)",
                 emp_name="wendy")
    conn.execute("insert into employee (emp_name) values (:emp_name)",
                 emp_name="wendy")
    trans.commit()

    conn.close()

    # transaction also works with
    with engine.begin() as conn:
        conn.execute("insert into employee (emp_name) values (:emp_name)",
                     emp_name="wendy")
        conn.execute("insert into employee (emp_name) values (:emp_name)",
                     emp_name="wendy")


"""
DDL, metadata.

Metadata/Schema:
    - describe the structure of database in terms of python structure.
    - Generate to and from schema.

`Metadata` as a collection of mutiple `Table`.
`Table` is a object representation of database table
 calling Table registers the Table to Metadata directly.
"""


def ddl_fn1():
    """ Metadata that describe db table """
    meta = MetaData()
    user_table = Table(
        'user', meta,
        Column('id', Integer, primary_key=True),
        Column('name', String),
        Column('fullname', String))

    # .c is an associative array of column objects
    print(user_table.c.name)   # [out] Column('name', String)
    print(user_table.c.id)

    # each column has it's attributes.
    name_col: Column = user_table.c.name
    print(name_col.type)    # String
    print(name_col.name)    # 'name'

    # The table object is the core of sql expression
    # this will output a string of sql.
    print(user_table.select())  # [out] SELECT "user".id,  ...


def ddl_fn2():
    engine = create_engine("sqlite://")

    meta = MetaData()
    user_table = Table(
        'user', meta,
        Column('id', Integer, primary_key=True),
        Column('name', String),
        Column('fullname', String),
        Column('price', Numeric(10, 2)),
        Column('type', Enum('a', 'b', 'c')))

    # table meta data allows for constraints and indexes.
    # ForeignKey is used to link one column to a remote
    # primary key.
    address_table = Table(
        'address', meta,
        Column('id', Integer, primary_key=True),
        Column('email_address', String(100), nullable=False),
        Column('user_id', Integer, ForeignKey('user.id')))

    # create  a schema in database.
    meta.create_all(engine)


def ddl_fn3():
    """
    Composite key as foregin key
    """

    meta = MetaData()
    story_table = Table(
        'story', meta,
        Column('story_id', Integer, primary_key=True),
        Column('version_id', Integer, primary_key=True),
        Column('headline', Unicode(100), nullable=False),
        Column('body', UnicodeText))

    published_table = Table(
        'published', meta,
        Column('pub_id', Integer, primary_key=True),
        Column('pub_time', DateTime, nullable=False),
        Column('story_id', Integer),
        Column('version_id', Integer),
        ForeignKeyConstraint(['story_id', 'version_id'],
                             ['story.story_id', 'story.version_id']))


def ddl_fn4():
    """
    reflection. Pull table into metadata from a existed database.
    """
    engine = create_engine("sqlite://")
    meta = MetaData()
    user_reflected = Table('user', meta, autoload=True, autoload_with=engine)
    print(user_reflected)

    # inspect information of a tale.
    inspector = inspect(engine)
    print(inspector.get_columns())


def ddl_fn5():
    """
    create drop
    """

    engine = create_engine("sqlite://")
    meta = MetaData()
    meta.create_all(engine)
    meta.drop_all(engine)

    user_reflected = Table('user', meta, autoload=True, autoload_with=engine)
    user_reflected.drop()


"""
 This is used either directly or throught the ORM.
 sql expression is build upon table matadata in order
 to compose sql statements in python.
 workflow:
    - build python object that represents individual sql statement.
    - the object is composed of other objects that each represents some
      unit of sql.
    - we work with this objects, which are than converted to strings when
      we 'execute' them.
"""


def sql_fn1():
    engine = create_engine("sqlite://")
    meta = MetaData()
    user_table = Table(
        'user', meta,
        Column('id', Integer, primary_key=True),
        Column('username', String(50)),
        Column('fullname', String(50)),
        Column('type', Enum('a', 'b', 'c')))
    meta.create_all(engine)

    # Type: sql.expression.BinaryExpression
    # mro of sql.expression.BinaryExpression will
    # resolve to operators of column elements.
    print(user_table.c.username == 'ed')
    print(str(user_table.c.username == 'ed'))

    # both | and and_ are logic operetion over
    # column elements. They serves the same purpose.
    print((user_table.c.username == 'ed')
          | (user_table.c.username == 'jack')
          | (user_table.c.username == 'ma'))
    print(
        and_(
            user_table.c.fullname == 'ed jones',
            or_(
                user_table.c.username == 'ed',
                user_table.c.username == 'jones',
                user_table.c.id > 10
            )
        ))
    print(user_table.c.username.in_(["wendy", "marry", "ed"]))


def sql_fn2():
    engine = create_engine("sqlite://")
    meta = MetaData()
    user_table = Table(
        'user', meta,
        Column('id', Integer, primary_key=True),
        Column('username', String(50)),
        Column('fullname', String(50)),
        Column('type', Enum('a', 'b', 'c')))
    meta.create_all(engine)

    # Column operator object.
    expression = (user_table.c.username == 'ed')
    print(expression.left)
    print(expression.right)
    print(expression.operator)  # from python operator lib

    # compile to sql.compiler.SQLCompiler
    compiled = expression.compile()
    # print embeded object into sql.
    print(compiled.params)


def sql_fn3():
    engine = create_engine("sqlite://")
    meta = MetaData()
    user_table = Table(
        'user', meta,
        Column('id', Integer, primary_key=True),
        Column('username', String(50)),
        Column('fullname', String(50)),
        Column('type', Enum('a', 'b', 'c')))

    address_table = Table(
        'address', meta,
        Column('id', Integer, primary_key=True),
        Column('email_address', String(100), nullable=False),
        Column('user_id', Integer, ForeignKey('user.id')))

    meta.create_all(engine)

    engine.execute(user_table
                   .select()
                   .where(user_table.c.username == 'ed'))

    engine.execute(user_table
                   .insert()
                   .values(username='ed', fullname='Ed Jones'))

    # some sql expressions
    with engine.connect() as conn:
        with conn.begin():
            conn.execute(user_table.insert(), [
                {'username': 'jack', 'fullname': 'Jack Burger'},
                {'username': 'wendy', 'fullname': 'Wendy Weathersmith'}
            ])

            d = conn.execute(user_table
                             .select()
                             .where((user_table.c.user_table == 'ed')
                                    | (user_table.c.user_table == 'wendy'))
                             .where(user_table.c.fullname != 'Sam')
                             .order_by(user_table.c.username)).fetchall()
            print(d)

            conn.execute(user_table
                         .join(address_table,
                               user_table.c.id == address_table.c.user_id))

            # subquery against "address" counts addresses per use.
            # select address.user_id, count(address.id) as count
            # from address group by address.user_id
            address_subq = select(
                [address_table.c.user_id,
                 func.count(address_table.c.id).label(
                     'count')
                 ]).group_by(address_table.c.user_id).alias()
            print(address_subq)

            # scalar select. the result is used as a part of another sql
            select_stmt = select(
                [user_table.c.username, address_subq.as_scalar()])
            print(select_stmt)

            update_stmt = (address_table
                           .update()
                           .values(email_address="j@a.com")
                           .where(address_table.c.email_address == "j@b.com"))
            print(update_stmt)


"""
    Object relational mapping.
        associating object oriented classes with database tables.
    object class as `domain model`

    +------------+              +------------+
    | Application|              |    db      |
    +------------+ class.save() +------------+
    |           ------------------>          |
    |            |              |            |
    | domain obj |              |  table row |
    |           <------------------          |
    |            | class.load() |            |
    +------------+              +------------+

    - most orm also repsent basic compositions, primarily one-to-many,
      many-to-one, use foregin key associations.
    - provide a mean of querying dtabase in terms of domain model
    - represent class inheritance hierachies
    - handle sharding of data. (store a domain model across multiple schemas)
    - provide various patterns for councurrency (row versioning)
    - provide pattern for data validation and coercion.

    // Two types of ORM
        Active record VS Data mapper.
    sqlalchemy is data mapper.

    <Key ORM pattern>
    | Unit of work. |
        : objects are maintained by a system that tracks changes over the
          course of a transaction, and flushes pending changes periodically,
          in a transparent manner.
    | Identity Map |
        : objs are tracked by their primary key within the unit of work, and
        are kept unique on that primary key identity. (one object for one
        primary key).
    | lazy loading |
        : Some attributes of an object may emit additional sql queries when
        they are accessed.
    | eager loading |
        : multple tables are quried at once in order to load related objects
        and collections.
    | method chaining |
        : ...
"""


class User(Base):
    __tablename__ = 'user'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    fullname = Column(String)

    def __repr__(self):
        return "<User (%r %r)>" % (self.name, self.fullname)


class Network(Base):
    __tablename__ = "network"
    network_id = Column(Integer, primary_key=True)
    name = Column(String(20), nullable=False)


"""
Joins and relationships
"""


class Adress(Base):
    __tablename__ = 'address'

    id = Column(Integer, primary_key=True)
    email_address = Column(String, nullable=False)
    user_id = Column(Integer, ForeignKey('user.id'))

    # backref set up Adress.user for each User.address.
    # establish a one to many relationship.
    user = relationship('User', backref='addresses')

    def __repr__(self):
        return '<Adress (%r)>' % self.email_address


def orm_fn1():
    # table object for user.
    print(User.__table__)

    # mediate relationship between user class and table
    print(User.__mapper__)


def orm_fn2():
    engine = create_engine("sqlite://")
    Base.metadta.create_all(engine)


def orm_fn3():
    """
    TO persist and load User Object from databse, we use
    Session objecet.

    engine has source of connection. it is typically used
    by being passed to other control objects
    """
    engine = create_engine("sqlite://")
    session = Session(bind=engine)

    # Add a new user, it's at PENDING state.
    # nothing get flush into database yet.
    ed_user = User(name="ed", fullname="Ed Jone")
    session.add(ed_user)
    print(session.new)

    # =========================================================
    # Session will flush PENDING objects into
    # database before each Query.
    # "AUTOFLUSH" // lazy. don't flush until it has to.
    our_user = session.query(User).filter_by(name='ed').first()
    print(our_user)

    # =========================================================
    # identity map. this two are actually the same.
    # as long as they are in the same session.
    #
    # flush is defered, so you need to make sure everything
    # in the same session see the same state, identity map
    # helps us to do that.
    print(ed_user is our_user)  # True

    # repeatable read ...

    session.add_all([
        User(name="Ed", fullname="Ed Je"),
        User(name="eD", fullname="Ed Joe")])

    ed_user.fullname = "Ed JoNES"
    print(session.new)
    print(session.dirty)   # ed_user is changed.

    # =========================================================
    # commit the whole transaction.
    # after commit there is no transaction. The
    # Session "invalidates" all data, so accessing them will
    # automatically start a new transaction and reload from the
    # database.
    session.commit()

    print(ed_user.__dict__)  # not is empty
    print(ed_user.fullname)  # new transaction started.
    print(ed_user.__dict__)  # object is mapped.

    # =========================================================
    # When their is no transaction heppening, Session assumes
    # it knows nothing about the data.
    # transaction is the real source of data.

    ed_user.fullname = "a"
    session.flush()  # forcibaly flush

    # ======= QUERY =============================================
    sel = (select([User.name, User.fullname])
           .where(User.name == 'ed')
           .order_by(User.id))
    # use session.connection() to pull out underneath connection
    res = session.connection().execute(sel).fetchall()
    print(res)

    # or
    query = session.query(User).filter(User.name == 'ed').order_by(User.id)
    print(query.all())

    # use as iterator
    for row in session.query(User, User.name):
        print(row.User, row.name)


def orm_fn4():
    engine = create_engine("sqlite://")
    session = Session(bind=engine)

    ed_user = User(name="ed", fullname="Ed Jone")
    # one to many relationship.
    ed_user.addresses = [
        Adress(email_address="asd@a.com"),
        Adress(email_address="asdi@a.com"),
        Adress(email_address="asd2@a.com"),
    ]

    # cascate rule: cascade objects and flush in the
    # right order.
    session.add(ed_user)

    # commit
    session.commit()

    # after commit, ed_user is gone (expiration.)
    # ed_user.addresses emits a "lazy load" when first
    # accessed.
    print(ed_user.addresses)

    # query with join
    query = (session
             .query(User, Adress)
             .join(Adress, User.id == Adress.user_id)
             .all())
    print(query)

    # alias
    a1, a2 = aliased(Adress)
    query = (session
             .query
             .join(a1)
             .join(a2)
             .filter(a1.email_address == 'asd@gmail.com')
             .filter(a2.email_address == "asd@b.com")
             .all())
    print(query)

    # subquery.
    subq = (session
            .query(func.count(Adress.id).label('count'),
                   User.id.label('user_id'))
            .join(Adress.user)
            .group_by(User.id)
            .subquery())

    query = (session   # use subquery in other queries.
             .query(User.name, func.coalesce(subq.c.count, 0))
             .outerjoin(subq, User.id == subq.c.user_id)
             .all())
    print(query)


"""
many-to-many
"""


class Employee(Base):
    __tablename__ = 'employee'

    id = Column(Integer, primary_key=True)
    name = Column(String(30))

    projects = relationship(
        'Project',
        secondary=Table(
            'employee_project',
            Base.metadata,
            Column('employee_id', Integer, ForeignKey('employee.id'),
                   primary_key=True),
            Column('employee_id', Integer, ForeignKey('employee.id'),
                   primary_key=True)),
        backref='employees')


class Project(Base):
    __tablename__ = 'project'
    id = Column(Integer, primary_key=True)
    name = Column(String(30))
