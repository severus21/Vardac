package com.lg4dc;

import java.util.Iterator;

public class IntegerRange implements Iterable<Integer> {
    private int start, end;

    public IntegerRange(int start, int end) {
        if (start <= end) {
            this.start = start;
            this.end = end;
        } else {
            this.start = end;
            this.end = start;
        }
    }

    @Override
    public Iterator<Integer> iterator() {
        return new IntegerRangeIterator();
    }

    private class IntegerRangeIterator implements Iterator<Integer> {
        private int current = start;

        @Override
        public boolean hasNext() {
            return current < end;
        }

        @Override
        public Integer next() {
            return current++;
        }
    }
}