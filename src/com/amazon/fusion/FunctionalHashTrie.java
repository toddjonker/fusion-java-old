// Copyright (c) 2018-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.util.hamt.HashArrayMappedTrie;
import com.amazon.fusion.util.hamt.HashArrayMappedTrie.Results;
import com.amazon.fusion.util.hamt.HashArrayMappedTrie.TrieNode;
import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * A functional hash table using a {@link HashArrayMappedTrie}.
 * <p>
 * This version is an internal implementation detail of FusionJava, is not intended
 * for reuse, and does not support Java nulls as keys or values. Attempts to use null
 * on operations such as {@link #get(Object)} or {@link #with(Object, Object)} will throw a
 * {@link NullPointerException}.
 * </p>
 * Iteration order of this data structure is undefined and not guaranteed to be stable.
 */
@SuppressWarnings({"unchecked", "rawtypes"})
class FunctionalHashTrie<K, V>
    implements Iterable<Entry<K, V>>
{
    private static final String NULL_ERROR_MESSAGE =
        "FunctionalHashTrie does not support null keys or values";

    private static final FunctionalHashTrie EMPTY =
        new FunctionalHashTrie<>(HashArrayMappedTrie.empty(), 0);
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


    static <K, V> FunctionalHashTrie<K, V> merge(Iterator<Entry<K, V>> items,
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

    static <K, V> FunctionalHashTrie<K, V> merge(Entry<K, V>[] items,
                                                 BiFunction<V, V, V> remapping)
    {
        return merge(Arrays.asList(items).iterator(), remapping);
    }


    FunctionalHashTrie(TrieNode<K, V> root, int size)
    {
        root.getClass(); // Null check

        this.root = root;
        this.size = size;
    }


    /**
     * @param key to examine the map for.
     * @return true if the key is in the map, false otherwise.
     */
    public boolean containsKey(K key)
    {
        return get(key) != null;
    }


    /**
     * @param key the key to search for.
     * @return the value associated with key, null it if is not in the map.
     */
    public V get(K key)
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
            return root.get((K) key);
        }
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

        Results results = new Results();
        TrieNode<K, V> newRoot = root.with(key, value, results);
        if (!results.modified())
        {
            return this;
        }

        int newSize = size + results.keyCountDelta();
        return new FunctionalHashTrie<>(newRoot, newSize);
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
            Results results = new Results();
            TrieNode<K, V> newRoot = root.without(key, results);
            if (!results.modified())
            {
                return this;
            }

            int newSize = size + results.keyCountDelta();
            return new FunctionalHashTrie<>(newRoot, newSize);
        }
    }


    @Override
    public Iterator<Entry<K, V>> iterator()
    {
        return root.iterator();
    }


    public Set<K> keySet()
    {
        // FIXME This is an extremely expensive implementation.
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


    public int size()
    {
        return size;
    }


    public boolean isEmpty()
    {
        return size == 0;
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
            return new MutableHashTrie(HashArrayMappedTrie.empty(), 0);
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
            TrieNode<K, V> newRoot = root.mWith(key, value, results);
            root = newRoot;
            size += results.keyCountDelta();

            return this;
        }


        private V get(K key)
        {
            if (key == null)
            {
                throw new NullPointerException(NULL_ERROR_MESSAGE);
            }

            return root.get((K) key);
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
