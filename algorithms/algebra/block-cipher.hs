module BlockCipher where

-- block cipher
-- F(n, p) is a vector space, because Zp is a field
-- Z(n, m) is not necessarily a vector space.
--
-- But turns out it still have some properties of a vector space.
-- You can calculate the inverse of a matrix in Z(n, m). In a vector space
-- a matrix M has inverse iff det(M) != 0.
--
-- But in a ring it becomes det(M) is a unit of the ring.
-- Which makes perfect sense because it's exactly the same thing as saying
-- it's not zero in a field, since all elements in a field is non zero.
--
-- in a 'semi vector space' Z(n, m), we have matrix M invertable iff det(M) is a unit
-- in Zm, iff gcd(det(M), m) = 1.
--
-- Another way to phrase it is:
--  if M h has inverse matrix, det(M) must have inverse in the ring Zm.

-- hill cipher is a block cipher, meaning you encrypt a block of text at a time.
--
--
