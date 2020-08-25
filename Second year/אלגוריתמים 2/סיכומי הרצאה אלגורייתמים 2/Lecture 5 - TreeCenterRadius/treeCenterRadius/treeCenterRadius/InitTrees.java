package treeCenterRadius;

public class InitTrees {
    public static MyVector[] initTree1() {
        int n = 7;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        ////////////
        tree[1].add(0);
        tree[1].add(2);
        tree[1].add(4);
        ///////////
        tree[2].add(1);
        tree[2].add(3);
        /////////////
        tree[3].add(2);
        ////////////
        tree[4].add(1);
        tree[4].add(5);
        ////////////
        tree[5].add(4);
        tree[5].add(6);
        ////////////
        tree[6].add(5);
        return tree;
    }

    public static MyVector[] initTree2() {//Graph: 0<->1<->2<->3<->4
        int n = 5;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        ////////////
        tree[1].add(0);
        tree[1].add(2);
        ///////////
        tree[2].add(1);
        tree[2].add(3);
        /////////////
        tree[3].add(2);
        tree[3].add(4);
        ////////////
        tree[4].add(3);
        return tree;
    }

    public static MyVector[] initTree3() {//Graph: 0<->1<->2<->3<->4<->5<->6<->7
        int n = 8;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        ////////////
        tree[1].add(0);
        tree[1].add(2);
        ///////////
        tree[2].add(1);
        tree[2].add(3);
        /////////////
        tree[3].add(2);
        tree[3].add(4);
        ////////////
        tree[4].add(3);
        tree[4].add(5);
        ////////////
        tree[5].add(4);
        tree[5].add(6);
        ////////////
        tree[6].add(5);
        tree[6].add(7);
        ////////////
        tree[7].add(6);
        return tree;
    }

    public static MyVector[] initTree4() {
        int n = 8;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        /////
        tree[1].add(0);
        tree[1].add(2);
        ////////////
        tree[2].add(1);
        tree[2].add(3);
        tree[2].add(5);
        ///////////
        tree[3].add(2);
        tree[3].add(4);
        /////////////
        tree[4].add(3);
        ////////////
        tree[5].add(2);
        tree[5].add(6);
        ////////////
        tree[6].add(5);
        tree[6].add(7);
        ////////////
        tree[7].add(6);
        return tree;
    }

    public static MyVector[] initTree5() {
        int n = 7;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        ////////////
        tree[1].add(0);
        tree[1].add(2);
        ///////////
        tree[2].add(1);
        tree[2].add(4);
        tree[2].add(6);
        /////////////
        tree[3].add(5);
        ////////////
        tree[4].add(2);
        tree[4].add(5);
        ////////////
        tree[5].add(4);
        tree[5].add(3);
        ////////////
        tree[6].add(2);
        return tree;
    }

    public static MyVector[] initTree6() {
        int n = 7;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        tree[0].add(2);
        tree[0].add(3);
        tree[0].add(4);
        tree[0].add(5);
        tree[0].add(6);
        tree[1].add(0);
        tree[2].add(0);
        tree[3].add(0);
        tree[4].add(0);
        tree[5].add(0);
        tree[6].add(0);
        return tree;
    }

    public static MyVector[] initTree7() {//Graph: 0<->1<->2<->3<->4
        int n = 4;
        MyVector[] tree = new MyVector[n];//graph (tree)array of vectors
        for (int i = 0; i < n; i++) {
            tree[i] = new MyVector(n);
        }
        tree[0].add(1);
        ////////////
        tree[1].add(0);
        tree[1].add(2);
        ///////////
        tree[2].add(1);
        tree[2].add(3);
        /////////////
        ////////////
        tree[3].add(2);
        return tree;
    }
}


