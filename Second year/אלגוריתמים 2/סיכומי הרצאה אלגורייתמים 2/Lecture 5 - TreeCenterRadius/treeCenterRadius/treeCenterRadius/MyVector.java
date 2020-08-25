package treeCenterRadius;



public class MyVector {
    int[] _data;
    int _size;

    public MyVector(int size) {
        _data = new int[size];
        _size = 0;
    }

    public void add(int val) {
        _data[_size++] = val;
    }

    public int get(int index) {
        return _data[index];
    }

    public void set(int index, int val) {
        _data[index] = val;
    }

    public int size() {
        return _size;
    }

    public void remove(int x) {

    }

    public String toString() {
        String ans = "";
        if (_size == 1) ans = ans + _data[0];
        else {
            for (int i = 0; i < _size; i++) {
                ans = ans + _data[i] + ", ";
            }
        }
        return ans;
    }
}
