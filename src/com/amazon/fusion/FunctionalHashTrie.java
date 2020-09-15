// Copyright (c) 2018-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.HashArrayMappedTrie.hashCodeFor;
import com.amazon.fusion.HashArrayMappedTrie.FlatNode;
import com.amazon.fusion.HashArrayMappedTrie.Results;
import com.amazon.fusion.HashArrayMappedTrie.TrieNode;
import java.util.AbstractCollection;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * A functional {@link Map} using a
 * <a href="https://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf">Hash Array Mapped Trie</a>.
 *
 * This version is an internal implementation detail of FusionJava, is not intended
 * for reuse, and does not support Java nulls as keys or values. Attempts to use null
 * on operations such as {@link #get(Object)} or {@link #with(Object, Object)} will throw a
 * {@link NullPointerException} as per {@link Map} specifications.
 *
 * Iteration order of this data structure is undefined.
 */
@SuppressWarnings({"unchecked", "rawtypes"})
class FunctionalHashTrie<K, V>
    extends AbstractMap<K, V> implements Iterable<Map.Entry<K, V>>
{
    private static final String NULL_ERROR_MESSAGE =
        "FunctionalHashTrie does not support null keys or values";

    private static final FunctionalHashTrie EMPTY =
        new FunctionalHashTrie<>(null, 0);
    private final TrieNode<K, V> root;
    private final int size;

    static <K, V> FunctionalHashTrie<K, V> empty()
    {
        return EMPTY;
    }

    static <K, V> FunctionalHashTrie<K, V> create(Map<K, V> other)
    {
        if (other instanceof FunctionalHashTrie)
        {
            return (FunctionalHashTrie) other;
        }

        MutableHashTrie<K, V> ret = MutableHashTrie.makeEmpty();
        for (Entry<K, V> entry : other.entrySet())
        {
            ret = ret.mWith(entry.getKey(), entry.getValue());
        }

        return ret.asFunctional();
    }


    static <K, V> FunctionalHashTrie<K, V> merge(Iterator<Map.Entry<K, V>> items,
                                                 BiFunction<V, V, V> remapping)
    {
        MutableHashTrie<K, V> ret = MutableHashTrie.makeEmpty();
        while (items.hasNext())
        {
            Entry<K, V> item = items.next();

            // TODO: Improve performance by having MutableHashTrie perform the merge operation itself.
            V prev = ret.get(item.getKey());
            if (prev != null)
            {
                ret.mWith(item.getKey(), remapping.apply(prev, item.getValue()));
            }
            else
            {
                ret.mWith(item.getKey(), item.getValue());
            }
        }

        return ret.asFunctional();
    }

    static <K, V> FunctionalHashTrie<K, V> merge(Map.Entry<K, V>[] items,
                                                 BiFunction<V, V, V> remapping)
    {
        return merge(Arrays.asList(items).iterator(), remapping);
    }


    FunctionalHashTrie(TrieNode<K, V> root, int size)
    {
        this.root = root;
        this.size = size;
    }


    /**
     * @param key to examine the map for.
     * @return true if the key is in the map, false otherwise.
     */
    @Override
    public boolean containsKey(Object key)
    {
        return get(key) != null;
    }


    /**
     * Warning: THIS IS REALLY SLOW.
     */
    @Override
    public boolean containsValue(Object value)
    {
        if (value == null)
        {
            throw new NullPointerException(NULL_ERROR_MESSAGE);
        }
        return values().contains(value);
    }


    /**
     * @param key the key to search for.
     * @return the value associated with key, null it if is not in the map.
     */
    @Override
    public V get(Object key)
    {
        if (key == null)
        {
            throw new NullPointerException(NULL_ERROR_MESSAGE);
        }

        if (isEmpty())
        {
            return null;
        }
        else
        {
            return root.get(hashCodeFor(key), 0, key);
        }
    }


    /**
     * put is not supported since it implicitly requires mutability.
     */
    @Override
    public V put(K key, V value)
    {
        throw new UnsupportedOperationException();
    }

    // TODO: Add a variation of with[out] that returns the previous value (if any).

    /**
     * This method functionally modifies the {@link FunctionalHashTrie} and returns
     * a new {@link FunctionalHashTrie} if a modification was made, otherwise returns itself.
     *
     * The equivalent of Clojure's PersistentHashMap's assoc().
     */
    public FunctionalHashTrie<K, V> with(K key, V value)
    {
        if (key == null || value == null)
        {
            throw new NullPointerException(NULL_ERROR_MESSAGE);
        }

        TrieNode<K, V> newRoot;
        if (root == null)
        {
            newRoot = new FlatNode<>(key, value);
            return new FunctionalHashTrie<>(newRoot, 1);
        }

        Results results = new Results();
        newRoot = root.with(hashCodeFor(key), 0, key, value, results);
        if (newRoot == root)
        {
            return this;
        }
        return new FunctionalHashTrie<>(newRoot,
                                        ! results.modified()
                                                        ? size
                                                        : size + 1);
    }


    /**
     * This method functional removes a key from the {@link FunctionalHashTrie} and returns
     * a new {@link FunctionalHashTrie} if a modification was made, otherwise returns itself.
     */
    public FunctionalHashTrie<K, V> without(K key)
    {
        if (isEmpty())
        {
            return this;
        }
        else if (key == null)
        {
            throw new NullPointerException(NULL_ERROR_MESSAGE);
        }
        else
        {
            TrieNode<K, V> newRoot = root.without(hashCodeFor(key), 0, key);
            if (newRoot == root)
            {
                return this;
            }
            else
            {
                return new FunctionalHashTrie<>(newRoot,
                                                size - 1);
            }
        }
    }


    /**
     * remove is not supported because it implicitly requires mutability.
     */
    @Override
    public V remove(Object key)
    {
        throw new UnsupportedOperationException();
    }


    /**
     * putAll is not supported because it implicitly requires mutability.
     */
    @Override
    public void putAll(Map<? extends K, ? extends V> m)
    {
        throw new UnsupportedOperationException();
    }


    /**
     * clear is not supported because it implicitly requires mutability.
     */
    @Override
    public void clear()
    {
        throw new UnsupportedOperationException();
    }


    private static final Iterator EMPTY_ITERATOR = new Iterator()
    {
        @Override
        public boolean hasNext()
        {
            return false;
        }

        @Override
        public Object next()
        {
            throw new NoSuchElementException();
        }

        @Override
        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    };


    @Override
    public Iterator<Entry<K, V>> iterator()
    {
        if (isEmpty())
        {
            return EMPTY_ITERATOR;
        }
        return root.iterator();
    }


    @Override
    public Set<K> keySet()
    {
        return new AbstractSet<K>()
        {
            @Override
            public Iterator<K> iterator()
            {
                final Iterator<Entry<K, V>> entryIter = FunctionalHashTrie.this.iterator();
                return new Iterator<K>()
                {
                    @Override
                    public boolean hasNext()
                    {
                        return entryIter.hasNext();
                    }

                    @Override
                    public K next()
                    {
                        return entryIter.next().getKey();
                    }

                    @Override
                    public void remove()
                    {
                        throw new UnsupportedOperationException();
                    }
                };
            }

            @Override
            public int size()
            {
                return size;
            }
        };
    }


    @Override
    public Collection<V> values()
    {
        return new AbstractCollection<V>()
        {
            @Override
            public Iterator<V> iterator()
            {
                final Iterator<Entry<K, V>> entryIter = FunctionalHashTrie.this.iterator();
                return new Iterator<V>()
                {
                    @Override
                    public boolean hasNext()
                    {
                        return entryIter.hasNext();
                    }

                    @Override
                    public V next()
                    {
                        return entryIter.next().getValue();
                    }

                    @Override
                    public void remove()
                    {
                        throw new UnsupportedOperationException();
                    }
                };
            }

            @Override
            public int size()
            {
                return size;
            }
        };
    }


    @Override
    public Set<Entry<K, V>> entrySet()
    {
        return new AbstractSet<Entry<K, V>>()
        {
            @Override
            public Iterator<Entry<K, V>> iterator()
            {
                return FunctionalHashTrie.this.iterator();
            }

            @Override
            public int size()
            {
                return size;
            }
        };
    }


    @Override
    public int size()
    {
        return size;
    }


    @Override
    public boolean isEmpty()
    {
        return size == 0;
    }

    /**
     * Visible for testing purposes only. Use this at your own demise.
     */
    TrieNode<K, V> getRoot()
    {
        return root;
    }


    // TODO: Add method that accepts a function to modify each element in the trie.

    /**
     * Mutable version of {@link FunctionalHashTrie} for faster and more memory
     * efficient instantiation of new {@link FunctionalHashTrie}s.
     */
    private static class MutableHashTrie<K, V>
    {
        private int size;
        private TrieNode<K, V> root;

        private static MutableHashTrie makeEmpty()
        {
            return new MutableHashTrie(new FlatNode(new Object[0]), 0);
        }

        private MutableHashTrie(TrieNode<K, V> root,
                                int size)
        {
            this.root = root;
            this.size = size;
        }

        private MutableHashTrie<K, V> mWith(K key, V value)
        {
            if (key == null || value == null)
            {
                throw new NullPointerException(NULL_ERROR_MESSAGE);
            }

            Results results = new Results();
            TrieNode<K, V> newRoot = root.mWith(hashCodeFor(key), 0, key, value, results);
            if (newRoot != root)
            {
                root = newRoot;
            }
            if (results.modified())
            {
                size++;
            }

            return this;
        }


        private V get(Object key)
        {
            if (key == null)
            {
                throw new NullPointerException(NULL_ERROR_MESSAGE);
            }

            if (size == 0)
            {
                return null;
            }
            else
            {
                return root.get(hashCodeFor(key), 0, key);
            }
        }


        /**
         * This should be called after the desired mutations on the trie are
         * performed. Drops the current root, so that further {@link #mWith(Object, Object)} calls fail.
         */
        FunctionalHashTrie<K, V> asFunctional()
        {
            FunctionalHashTrie ret;
            if (size == 0)
            {
                ret = FunctionalHashTrie.EMPTY;
            }
            else
            {
                ret = new FunctionalHashTrie(root, size);
            }
            root = null;
            return ret;
        }
    }
}
