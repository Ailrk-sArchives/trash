

if __name__ == "__main__":

    for i in range(0, 4):
        for j in range(0, 4):
            for k in range(0, 4):
                if k > 2:
                    break
                print(i, j, k)
                print("kk")
            print("jj")
        print("ii")

