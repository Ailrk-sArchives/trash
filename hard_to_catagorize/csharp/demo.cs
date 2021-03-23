using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace Learning.CSharp
{
    public class LearnCSharp
    {
        public static void Syntax()
        {
            Console.WriteLine("Hello World");
            Console.WriteLine("Integer" + 10);

            sbyte foo = 100;
            byte foo1 = 100;

            short fooShort = 10000;
            ushort foo2 = 1000;

            DateTime foodate = DateTime.Now;

            // array
            int[] intArray = new int[10];
            intArray[1] = 1;

            // List
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            intList.Add(10);
            int i1 = 1, i2 = 2;

            foreach (char ch in "hello".ToCharArray())
            {
                Console.WriteLine("char is: " + ch);
            }
            int a = int.Parse("123");
            Convert.ToSByte(123);
        }

        // pass by ref and use as output.
        public static void Msign(ref int maxCount, out int count)
        {
            count = 14;

        }

        public static V SetDefault<K, V>(IDictionary<K, V> dictionary, K key, V defaultItem)
        {
            V result;
            if (!dictionary.TryGetValue(key, out result))
            {
                return dictionary[key] = defaultItem;
            }
            return result;
        }

        // iterator with foreach support
        // it's bascially a generator.
        public static IEnumerable<int> YieldCounter(int limit = 10)
        {
            for (var i = 0; i < limit; ++i)
            {
                yield return i;
            }
        }

        public static void PrintYieldCounterConsole()
        {
            foreach (var counter in YieldCounter())
            {
                Console.WriteLine(counter);
            }
        }

        public static void Others()
        {
            // optional
            int? nullable = null;
            int notNull = nullable ?? 0;

            // any type
            // this is crazy
            dynamic student = new List<int>();
            student = 10;
        }

        public delegate int IncrementDelegate();
    }

}
