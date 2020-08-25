package treeCenterRadius;//package treeCenterRadius;

public class Fire {
    public static int fire(MyVector[] tree) {
        int n = tree.length;
        MyVector leaves = new MyVector(n);
        int radius = 0, diameter = 0, numCenters = 0;
        int[] degrees = new int[n];
        // initialization, n - number of vertices
        for (int i = 0; i < n; i++) {
            degrees[i] = tree[i].size();
            if (degrees[i] == 1) {
                leaves.add(i);
            }
        }
        // calculations: find Centers, Radius and Diameter
        int vertex = 0, leaf = 0;
        while (n > 2) {
            MyVector temp = new MyVector(n);
            for (int i = 0; i < leaves.size(); i++) {
                leaf = leaves.get(i);
                degrees[leaf] = 0;
                for (int j = 0; j < tree[leaf].size(); j++) {
                    vertex = tree[leaf].get(j);
                    if (degrees[vertex] > 0) {
                        degrees[vertex]--;
                        if (degrees[vertex] == 1) temp.add(vertex);
                    }
                }
                n--;
            }
            leaves = temp;
            radius++;
        }
        if (leaves.size() == 2) {
            radius++;
            diameter = radius * 2 - 1;
        } else diameter = radius * 2;
        numCenters = leaves._size;
        System.out.println("radius = " + radius + ", diameter = " + diameter + ", centers: " + leaves.toString()
                + " numCenters = " + numCenters);
        return diameter;
    }

    public static void main(String[] args) {
        fire(InitTrees.initTree1());//radius = 3, diameter = 5, centers: 1, 4
        fire(InitTrees.initTree2());//radius = 2, diameter = 4, centers: 2
        fire(InitTrees.initTree3());//radius = 4, diameter = 7, centers: 3, 4
        fire(InitTrees.initTree4());//radius = 3, diameter = 5, centers: 2, 5
        fire(InitTrees.initTree5());//radius = 3, diameter = 5, centers: 2, 4
        fire(InitTrees.initTree6());//radius = 1, diameter = 2, centers: 0
        fire(InitTrees.initTree7());//radius = 2, diameter = 3, centers: 1, 2
    }

}
