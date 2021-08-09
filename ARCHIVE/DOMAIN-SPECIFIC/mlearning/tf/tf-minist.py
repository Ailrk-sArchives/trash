import tensorflow as tf
from tensorflow.examples.tutorials.mnist import input_data

minst = input_data.read_data_sets("MNIST_data", one_hot=True)

# declare global vars.
IMG_SZ = 28
LABEL_SZ = 10
LEARNING_RATE = 0.05
STEP_N = 2000
BATCH_SZ = 100

# model y = Wx + b.
# define placeholders.
training_data = tf.placeholder(tf.float32, [None, IMG_SZ**2])
labels = tf.placeholder(tf.float32, [None, LABEL_SZ])

# variables to be tuned.
W = tf.Variable(tf.truncated_normal([IMG_SZ**2, LABEL_SZ], stddev=0.1))
b = tf.Variable(tf.constant(0.1, shape=[LABEL_SZ]))

# build the output layer of the network.
output = tf.matmul(training_data, W) + b

# training process works by optimizing the loss function.
# A common technique: Cross entropy.
loss = tf.reduce_mean(
        tf.nn.softmax_cross_entropy_with_logits(
            labels=labels, logits=output
            )
        )

# train step. GradientDescent oper.
optimizer = tf.train.GradientDescentOptimizer(LEARNING_RATE).minimize(loss)

#
# compare which labels we predicted correct with argmax(.).
correct_prediction = tf.equal(tf.argmax(output, 1), tf.argmax(labels, 1))
accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))

# The actual training session.
sess = tf.InteractiveSession()
sess.run(tf.global_variables_initializer())

for i in range(STEP_N):
    # get the next batch.
    input_batch, labels_batch = minst.train.next_batch(BATCH_SZ)
    feed_dict = {training_data: input_batch, labels: labels_batch}

    optimizer.run(feed_dict=feed_dict)

    if i % 100 == 0:
        train_accuracy = accuracy.eval(feed_dict=feed_dict)
        print("Step %d, training batch accuracy {} {} ".format(i, train_accuracy * 100))
