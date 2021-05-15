// Copyright (c) 2018-2021 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.util.hamt.HashArrayMappedTrie;
import com.amazon.fusion.util.hamt.HashArrayMappedTrie.TrieNode;
import java.util.AbstractSet;
import java.util.Arrays;
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

    // Mostly here so consumers don't have to use HashArrayMappedTrie.
    static class Changes
        extends HashArrayMappedTrie.Changes
    {
    }


    static <K, V> FunctionalHashTrie<K, V> empty()
    {
        return EMPTY;
    }

    static <K, V> FunctionalHashTrie<K, V> create(Map<K, V> other)
    {
        Changes changes = new Changes();
        TrieNode<K, V> trie = HashArrayMappedTrie.fromMap(other, changes);
        return EMPTY.resultFrom(trie, 0, 0, changes);
    }


    static <K, V> FunctionalHashTrie<K, V> fromEntries(Iterator<Entry<K, V>> items,
                                                       Changes changes)
    {
        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> trie = HashArrayMappedTrie.fromEntries(items, changes);

        return EMPTY.resultFrom(trie, priorChangeCount, priorKeyCountDelta, changes);
    }

    static <K, V> FunctionalHashTrie<K, V> fromEntries(Entry<K, V>[] items,
                                                       Changes changes)
    {
        return fromEntries(Arrays.asList(items).iterator(), changes);
    }

    static <K, V> FunctionalHashTrie<K, V> fromArrays(K[] keys,
                                                      V[] values,
                                                      Changes changes)
    {
        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> trie = HashArrayMappedTrie.fromArrays(keys, values, changes);

        return EMPTY.resultFrom(trie, priorChangeCount, priorKeyCountDelta, changes);
    }

    /**
     * Creates a trie by copying entries for the {@code keys} from the
     * {@code origin} trie.
     */
    static <K, V> FunctionalHashTrie<K, V>
    fromSelectedKeys(FunctionalHashTrie<K, V> origin, K[] keys)
    {
        return fromSelectedKeys(origin, keys, new Changes());
    }

    static <K, V> FunctionalHashTrie<K, V>
    fromSelectedKeys(FunctionalHashTrie<K, V> origin, K[] keys, Changes changes)
    {
        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> trie =
                HashArrayMappedTrie.fromSelectedKeys(origin.root, keys, changes);
        return EMPTY.resultFrom(trie, priorChangeCount, priorKeyCountDelta, changes);
    }

    private FunctionalHashTrie(TrieNode<K, V> root, int size)
    {
        this.root = root;
        this.size = size;
    }


    public int size()
    {
        return size;
    }

    public boolean isEmpty()
    {
        return size == 0;
    }


    @Override
    public Iterator<Entry<K, V>> iterator()
    {
        return root.iterator();
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

        return root.get(key);
    }


    private FunctionalHashTrie<K, V> resultFrom(TrieNode<K, V> newRoot,
                                                int priorChangeCount,
                                                int priorKeyCountDelta,
                                                Changes changes)
    {
        if (changes.changeCount() != priorChangeCount)
        {
            int newSize = size + changes.keyCountDelta() - priorKeyCountDelta;
            if (newSize == 0) return EMPTY;
            return new FunctionalHashTrie<>(newRoot, newSize);
        }

        assert root == newRoot;
        return this;
    }

    /**
     * This method functionally modifies the {@link FunctionalHashTrie} and returns
     * a new {@link FunctionalHashTrie} if a modification was made, otherwise returns itself.
     *
     * The equivalent of Clojure's PersistentHashMap's assoc().
     */
    public FunctionalHashTrie<K, V> with(K key, V value)
    {
        return with(key, value, new Changes());
    }

    /**
     * Functionally modify this trie, allowing for custom mappings.
     */
    public FunctionalHashTrie<K, V> with(K key, V value, Changes changes)
    {
        if (key == null || value == null)
        {
            throw new NullPointerException(NULL_ERROR_MESSAGE);
        }

        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> newRoot = root.with(key, value, changes);
        return resultFrom(newRoot, priorChangeCount, priorKeyCountDelta, changes);
    }


    public FunctionalHashTrie<K, V> merge(FunctionalHashTrie<K, V> that,
                                          Changes changes)
    {
        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> newRoot = root.with(that.iterator(), changes);
        return resultFrom(newRoot, priorChangeCount, priorKeyCountDelta, changes);
    }


    /**
     * This method functional removes a key from the {@link FunctionalHashTrie} and returns
     * a new {@link FunctionalHashTrie} if a modification was made, otherwise returns itself.
     */
    public FunctionalHashTrie<K, V> without(K key)
    {
        if (key == null)
        {
            throw new NullPointerException(NULL_ERROR_MESSAGE);
        }

        Changes changes = new Changes();
        TrieNode<K, V> newRoot = root.without(key, changes);
        return resultFrom(newRoot, 0, 0, changes);
    }


    /**
     * Functionally removes multiple keys from this trie.
     *
     * @param keys must not be null.
     *
     * @return the resulting trie.
     */
    public FunctionalHashTrie<K, V> withoutKeys(K[] keys)
    {
        return withoutKeys(keys, new Changes());
    }

    /**
     * Functionally removes multiple keys from this trie.
     *
     * @param keys must not be null.
     *
     * @return the resulting trie.
     */
    public FunctionalHashTrie<K, V> withoutKeys(K[] keys, Changes changes)
    {
        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> newRoot = root.withoutKeys(keys, changes);
        return resultFrom(newRoot, priorChangeCount, priorKeyCountDelta, changes);
    }


    public FunctionalHashTrie<K, V> transform(BiFunction<K, V, V> xform)
    {
        return transform(xform, new Changes());
    }

    public FunctionalHashTrie<K, V> transform(BiFunction<K, V, V> xform, Changes changes)
    {
        int priorChangeCount   = changes.changeCount();
        int priorKeyCountDelta = changes.keyCountDelta();

        TrieNode<K, V> newRoot = root.transform(xform, changes);
        return resultFrom(newRoot, priorChangeCount, priorKeyCountDelta, changes);
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
}
