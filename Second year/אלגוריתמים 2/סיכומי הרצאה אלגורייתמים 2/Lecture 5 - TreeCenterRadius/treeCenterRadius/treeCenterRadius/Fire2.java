package treeCenterRadius;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;

public class Fire2 {
    public static void fire2(MyVector[] tree) {
        int n = tree.length;
        Queue<Integer> leaves = new ArrayBlockingQueue<>(n);
        int radius = 0, diameter = 0, numCenters = 0;
        int[] degrees = new int[n];
        int[] levels = new int[n];
        // queue initialization, n - number of vertices
        for (int i = 0; i < n; i++) {
            degrees[i] = tree[i].size();
            if (degrees[i] == 1) leaves.add(i);
        }
        int leaf = 0, vertex = 0, maxLevel = 0;
        while (!leaves.isEmpty()) {//O(n)
            leaf = leaves.poll();
            degrees[leaf] = 0;
            for (int j = 0; j < tree[leaf].size(); j++) {
                vertex = tree[leaf].get(j); //O(1)
                if (degrees[vertex] > 0) degrees[vertex]--;
                if (degrees[vertex] == 1) {
                    leaves.add(vertex);
                    levels[vertex] = levels[leaf] + 1;
                    maxLevel = Math.max(maxLevel, levels[vertex]);
                }
            }

        }
        ArrayList<Integer> centers = new ArrayList<>(2);
        for (int i = 0; i < n; i++) {
            if (levels[i] == maxLevel) centers.add(i);
        }
        numCenters = centers.size();
        //diameter = 2*maxLevel + numCenters - 1;
        //radius = (diameter + 1)/2;
        if (numCenters == 2) {
            radius = maxLevel + 1;
            diameter = 2 * radius - 1;
        } else {
            radius = maxLevel;
            diameter = 2 * radius;
        }

        System.out.println("radius = " + radius + ", diameter = " + diameter + ", centers: " + centers
                + ", numCenters = " + numCenters);
        System.out.println("levels: " + Arrays.toString(levels));
    }

    public static void main(String[] args) {
        fire2(InitTrees.initTree1());//radius = 3, diameter = 5, centers: 1, 4
        fire2(InitTrees.initTree2());//radius = 2, diameter = 4, centers: 2
        fire2(InitTrees.initTree3());//radius = 4, diameter = 7, centers: 3, 4
        fire2(InitTrees.initTree4());//radius = 3, diameter = 5, centers: 2, 5
        fire2(InitTrees.initTree5());//radius = 3, diameter = 5, centers: 2, 4
        fire2(InitTrees.initTree6());//radius = 1, diameter = 2, centers: 0
        fire2(InitTrees.initTree7());//radius = 2, diameter = 3, centers: 1, 2
    }

}
