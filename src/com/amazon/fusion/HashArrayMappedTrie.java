// Copyright (c) 2018-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.AbstractMap.SimpleEntry;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Random;

/**
 * Implementation of a
 * <a href="https://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf">Hash Array Mapped Trie</a>.
 *
 * This version is an internal implementation detail of FusionJava, is not intended
 * for reuse, and does not support Java nulls as keys or values.
 *
 * Iteration order of this data structure is undefined. All key hashes used within
 * {@link HashArrayMappedTrie} are shuffled differently in each JRE. All hashes used for
 * navigating through the node structure should be routed through {@link #hashCodeFor(Object)}.
 *
 * Based on Clojure's PersistentHashMap and as such this implementation
 *  Uses path copying for persistence.
 *  Uses {@link CollisionNode}s rather than extended hashing.
 *  Uses node polymorphism rather than conditional checks.
 */
@SuppressWarnings({"unchecked", "rawtypes"})
class HashArrayMappedTrie
{
    private static final int STOP_RELYING_ON_UNDEFINED_BEHAVIOR =
        new Random().nextInt();

    /**
     * Describes the results of a trie operation.
     */
    public static class Results
    {
        private boolean modified = false;

        public Results()
        {
        }

        /**
         * Indicates if the operation changed the content of the trie: a key/value was added or
         * removed, or a key's value was changed. This can be true even when the operation returns
         * the same root node.
         */
        public boolean modified()
        {
            return modified;
        }

        private void keyAdded()
        {
            modified = true;
        }
    }

    /**
     * Used to provide the hash code for a key in the {@link HashArrayMappedTrie}.
     * ALL trie node interfacing calls should use this method!!
     *
     * Visible for testing.
     * @param key Object to get the hash code for.
     * @return a consistently shuffled hash code.
     */
    static int hashCodeFor(Object key)
    {
        return key.hashCode() ^ STOP_RELYING_ON_UNDEFINED_BEHAVIOR;
    }

    // TODO: Add method that accepts a function to modify each element in the trie.


    /**
     * Abstract class interface for Nodes.
     * TODO: Convert to interface with default methods in Java 8+
     */
    static abstract class TrieNode<K, V>
    {
        /**
         * Functionally modifies the trie to have the desired mapping from key to value.
         * @return Itself if it was not modified, else a new trie with the modification.
         */
        abstract TrieNode<K, V> with(int hash,
                                     int shift,
                                     Object key,
                                     Object value,
                                     Results results);

        /**
         * Mutates the trie to have the desired mapping from key to value.
         * This must only be called while constructing a new Trie, when its known that
         * nodes are not shared.
         * @return Itself
         */
        abstract TrieNode<K, V> mWith(int hash,
                                      int shift,
                                      Object key,
                                      Object value,
                                      Results results);

        /**
         * @return The value with the given key, or null if it does not exist in the trie.
         */
        abstract V get(int hash, int shift, Object key);

        /**
         * Functionally removes the mapping associated with the key from the trie.
         * @return Itself if it was not modified, else a new trie with the modification.
         */
        abstract TrieNode<K, V> without(int hash, int shift, K key);

        /**
         * @return An iterator over all elements within the trie.
         */
        abstract Iterator<Entry<K, V>> iterator();

        static int hashFragment(int hash, int shift)
        {
            assert shift <= 30;
            return hash >>> shift & 0x1f;
        }

        static int bitPosition(int hashFragment)
        {
            return 1 << hashFragment;
        }
    }

    /**
     * {@link FlatNode} is an optimization of {@link BitMappedNode} for small nodes.
     * Empirically, a linear search is faster for nodes with <= 8 keys.
     *
     * Maintains the same invariant as {@link BitMappedNode}s:
     * Keys are stored in even indices, values at odd.
     * If there is a null value at an even index, that means the corresponding odd index is a {@link CollisionNode}.
     *
     * While this is a flat array of key-value pairs, they are in no defined order.
     */
    static class FlatNode<K, V>
        extends TrieNode<K, V>
    {
        /**
         * As mentioned in {@link FlatNode}'s Javadoc, empirically, a linear search performs just
         * as fast or faster than a bitmapped search when there are <= 8 elements.
         */
        private static final int BITMAP_CONVERSION_SIZE = 8;

        Object[] kvPairs;


        FlatNode(K key, V value)
        {
            this(new Object[]{key, value});
        }


        FlatNode(K key1, V val1, K key2, V val2)
        {
            this(new Object[]{key1, val1, key2, val2});
        }


        FlatNode(Object[] kvPairs)
        {
            this.kvPairs = kvPairs;
        }


        @Override
        TrieNode<K, V> with(int hash,
                            int shift,
                            Object key,
                            Object value,
                            Results results)
        {
            int index = linearSearch(hash, key);
            if (index != -1)
            {
                Object keyOrNull = kvPairs[index];
                Object valOrNode = kvPairs[index + 1];
                if (keyOrNull == null)
                {
                    CollisionNode<K, V> node = (CollisionNode<K, V>) valOrNode;
                    TrieNode newNode = node.with(hash, shift, key, value, results);
                    if (newNode == node)
                    {
                        return this;
                    }
                    else
                    {
                        return new FlatNode<>(cloneAndModify(kvPairs, index + 1, newNode));
                    }
                }
                else
                {
                    if (equivVals(valOrNode, value))
                    {
                        return this;
                    }
                    else
                    {
                        return new FlatNode<>(cloneAndModify(kvPairs, index + 1, value));
                    }
                }
            }
            else
            {
                if (kvPairs.length >= 2 * BITMAP_CONVERSION_SIZE)
                {
                    return expand(hash, shift, key, value, results);
                }
                else
                {
                    Object[] newPairs = cloneAndExtend(kvPairs, key, value);
                    results.keyAdded();
                    return new FlatNode<>(newPairs);
                }
            }
        }


        @Override
        TrieNode<K, V> mWith(int hash,
                             int shift,
                             Object key,
                             Object value,
                             Results results)
        {
            int index = linearSearch(hash, key);
            if (index != -1)
            {
                Object valOrNode = kvPairs[index + 1];
                if (kvPairs[index] == null)
                {
                    CollisionNode<K, V> node = (CollisionNode<K, V>) valOrNode;
                    TrieNode<K, V> newNode = node.mWith(hash, shift, key, value, results);
                    if (newNode != node)
                    {
                        kvPairs[index + 1] = newNode;
                    }
                }
                else
                {
                    if (!equivVals(valOrNode, value))
                    {
                        kvPairs[index + 1] = value;
                    }
                }
            }
            else
            {
                if (kvPairs.length >= 2 * BITMAP_CONVERSION_SIZE)
                {
                    return expand(hash, shift, key, value, results);
                }
                else
                {
                    kvPairs = cloneAndExtend(kvPairs, key, value);
                    results.keyAdded();
                }
            }

            return this;
        }


        @Override
        V get(int hash, int shift, Object key)
        {
            int index = linearSearch(hash, key);
            if (index != -1)
            {
                Object keyOrNull = kvPairs[index];
                Object valOrNode = kvPairs[index + 1];
                if (keyOrNull == null)
                {
                    return ((CollisionNode<K, V>) valOrNode).get(hash, shift, key);
                }
                else
                {
                    return (V) valOrNode;
                }
            }

            return null;
        }


        @Override
        TrieNode<K, V> without(int hash,
                               int shift,
                               K key)
        {
            int index = linearSearch(hash, key);
            if (index != -1)
            {
                if (kvPairs[index] == null)
                {
                    CollisionNode<K, V> node = (CollisionNode<K, V>) kvPairs[index + 1];
                    TrieNode newNode = node.without(hash, shift, key);
                    if (newNode == null)
                    {
                        return withoutPair(index);
                    }
                    else
                    {
                        return new FlatNode<>(cloneAndModify(kvPairs, index + 1,
                                                             newNode));
                    }
                }
                else
                {
                    return withoutPair(index);
                }
            }
            return this;
        }


        TrieNode<K, V> withoutPair(int index)
        {
            if (kvPairs.length == 2)
            {
                return null;
            }
            else
            {
                return makeSimilar(cloneAndRemovePair(kvPairs, index));
            }
        }


        TrieNode<K, V> makeSimilar(Object[] kvPairs)
        {
            return new FlatNode<>(kvPairs);
        }


        @Override
        Iterator<Entry<K, V>> iterator()
        {
            return new EntryArrayIterator<>(kvPairs);
        }


        int linearSearch(int hash, Object key)
        {
            for (int i = 0; i < kvPairs.length; i += 2)
            {
                Object keyOrNull = kvPairs[i];
                if (keyOrNull == null)
                {
                    CollisionNode<K, V> node = (CollisionNode<K, V>) kvPairs[i + 1];
                    if (node.hash == hash)
                    {
                        return i;
                    }
                }
                else if (equivKeys(keyOrNull, key))
                {
                    return i;
                }
            }
            return -1;
        }


        private TrieNode<K, V> expand(int hash,
                                      int shift,
                                      Object key,
                                      Object value,
                                      Results results)
        {
            // Give the new BitMappedNode some buffer space for faster mWith calls.
            // Supposing perfect hash distribution within the FlatNode, the new BitMappedNode's
            // array would require four extra allocations to perform expansion.
            // In the degenerate case of extremely similar hashes, this is a slight waste of memory.
            TrieNode<K, V> newNode = new BitMappedNode<>(0, new Object[8]);
            for (int i = 0; i < kvPairs.length; i += 2)
            {
                Object keyOrNull = kvPairs[i];
                Object valOrNode = kvPairs[i + 1];
                if (keyOrNull == null)
                {
                    newNode = newNode.mWith(((CollisionNode) valOrNode).hash,
                                            shift,
                                            null,
                                            valOrNode,
                                            results);
                }
                else
                {
                    newNode = newNode.mWith(hashCodeFor(keyOrNull),
                                            shift,
                                            keyOrNull,
                                            valOrNode,
                                            results);
                }
            }
            return newNode.mWith(hash, shift, key, value, results);
        }
    }


    /**
     * A CollisionNode stores kvPairs that have direct hash collisions.
     * This is to resolve instances where there are not enough hash bits to index further.
     *
     * It is an extension of {@link FlatNode} but does not store nodes
     * within itself and is never extended into a {@link BitMappedNode} (since that is pointless)
     */
    static class CollisionNode<K, V>
        extends FlatNode<K, V>
    {
        private final int hash;

        CollisionNode(int hash, Object[] kvPairs)
        {
            super(kvPairs);
            this.hash = hash;
        }


        @Override
        TrieNode<K, V> with(int hash,
                            int shift,
                            Object key,
                            Object value,
                            Results results)
        {
            if (hash == this.hash)
            {
                int index = linearSearch(hash, key);
                if (index != -1)
                {
                    Object storedValue = kvPairs[index + 1];
                    if (storedValue == value)
                    {
                        return this;
                    }
                    else
                    {
                        return makeSimilar(cloneAndModify(kvPairs,
                                                          index + 1,
                                                          value));
                    }
                }
                else
                {
                    Object[] newPairs = cloneAndExtend(kvPairs, key, value);
                    results.keyAdded();
                    return makeSimilar(newPairs);
                }
            }

            // If it doesn't share the hash, push this collision node down a level.
            results.keyAdded();
            return new FlatNode<>(new Object[]{null, this, key, value});
        }


        @Override
        TrieNode<K, V> mWith(int hash,
                             int shift,
                             Object key,
                             Object value,
                             Results results)
        {
            if (hash == this.hash)
            {
                int index = linearSearch(hash, key);
                if (index != -1)
                {
                    Object storedValue = kvPairs[index + 1];
                    if (storedValue != value)
                    {
                        kvPairs[index + 1] = value;
                    }
                }
                else
                {
                    kvPairs = cloneAndExtend(kvPairs, key, value);
                    results.keyAdded();
                }

                return this;
            }

            // If it doesn't share the hash, push this collision node down a level.
            results.keyAdded();
            return new FlatNode<>(new Object[]{null, this, key, value});
        }


        @Override
        int linearSearch(int hash, Object key)
        {
            for (int i = 0; i < kvPairs.length; i += 2)
            {
                Object keyOrNull = kvPairs[i];
                if (equivKeys(keyOrNull, key))
                {
                    return i;
                }
            }
            return -1;
        }


        @Override
        TrieNode<K, V> makeSimilar(Object[] kvPairs)
        {
            return new CollisionNode<>(hash, kvPairs);
        }

        @Override
        public String toString()
        {
            return "{\n" +
                   "  Node Hash: " + hash + ",\n" +
                   "  Nodes: " + Arrays.toString(kvPairs) + ",\n" +
                   "}";

        }
    }


    /**
     * {@link BitMappedNode}s are the equivalent of Clojure's PersistentHashMap's BitmapIndexedNode.
     *
     * Piggy-backing off of the Clojure implementation, kvPairs are stored in this node as follows:
     * If the even index is not null, then it is a key and the following index is the value.
     * Otherwise, the even index is null and the following index is potentially a Node
     *  (It's possible for two consecutive indices to be null due to optimizations that
     *  {@link #mWith} and {@link FlatNode#expand} perform).
     *
     * Note: {@link BitMappedNode}s are never packed back down into {@link FlatNode}s.
     * This is because a search through a FlatNode would no longer be linear if the pre-packed
     * {@link BitMappedNode} has another non-{@link CollisionNode} as a child.
     */
    static class BitMappedNode<K, V>
        extends TrieNode<K, V>
    {
        /**
         * We convert {@link BitMappedNode}s to {@link HashArrayMappedNode}s after they reach
         * a size of 16 elements. Empirically, this conversion provides a performance gain
         * of up to 20% on {@link TrieNode#get} and {@link TrieNode#with}
         * operations as the {@link HashArrayMappedTrie} grows with randomly distributed keys.
         */
        private static final int HAMN_CONVERSION_SIZE = 16;

        static final TrieNode EMPTY = new BitMappedNode(0, new Object[0]);
        int bitmap;
        Object[] kvPairs; // Can be either a Key Value Pair or a Null then Node Pair.


        BitMappedNode(int bitmap, Object[] kvPairs)
        {
            this.bitmap = bitmap;
            this.kvPairs = kvPairs;
        }


        @Override
        TrieNode<K, V> with(int hash,
                            int shift,
                            Object key,
                            Object value,
                            Results results)
        {
            int hashFragment = hashFragment(hash, shift);
            int bit = bitPosition(hashFragment);
            int keyIndex = 2 * arrayIndex(bit);
            if (isInBitmap(bit))
            {
                Object keyOrNull = kvPairs[keyIndex];
                Object valOrNode = kvPairs[keyIndex + 1];
                if (keyOrNull == null)
                {
                    TrieNode<K, V> newNode =
                        ((TrieNode<K, V>) valOrNode)
                            .with(hash, shift + 5, key, value, results);
                    if (newNode == valOrNode)
                    {
                        return this;
                    }
                    else
                    {
                        return new BitMappedNode<>(bitmap,
                                                   cloneAndModify(kvPairs,
                                                                  keyIndex + 1,
                                                                  newNode));
                    }
                }
                else if (equivKeys(keyOrNull, key))
                {
                    if (equivVals(valOrNode, value))
                    {
                        return this;
                    }
                    else
                    {
                        return new BitMappedNode<>(bitmap,
                                                   cloneAndModify(kvPairs,
                                                                  keyIndex + 1,
                                                                  value));
                    }
                }
                else
                {
                    results.keyAdded();
                    FlatNode newNode = resolveCollision(hashCodeFor(keyOrNull),
                                                        keyOrNull,
                                                        valOrNode,
                                                        hash,
                                                        key,
                                                        value);
                    return new BitMappedNode<>(bitmap,
                                               cloneAndModify(kvPairs,
                                                              keyIndex,
                                                              null,
                                                              keyIndex + 1,
                                                              newNode));
                }
            }
            else
            {
                results.keyAdded();
                int numVals = Integer.bitCount(bitmap);
                if (numVals >= HAMN_CONVERSION_SIZE)
                {
                    return expand(key, value, hashFragment, numVals);
                }
                else
                {
                    Object[] newPairs = cloneExtendAndInsert(kvPairs,
                                                              numVals,
                                                              2 * (numVals + 1),
                                                              keyIndex,
                                                              key,
                                                              value);
                    return new BitMappedNode<>(bitmap | bit, newPairs);
                }
            }
        }


        /**
         * {@inheritDoc}
         *
         * In addition to mutating the trie, mWith allocates some extra space
         * to the {@link BitMappedNode} so that it can add nodes without needing
         * to expand the array as much.
         */
        @Override
        TrieNode<K, V> mWith(int hash,
                             int shift,
                             Object key,
                             Object value,
                             Results results)
        {
            int hashFragment = hashFragment(hash, shift);
            int bit = bitPosition(hashFragment);
            int keyIndex = 2 * arrayIndex(bit);
            if (isInBitmap(bit))
            {
                Object keyOrNull = kvPairs[keyIndex];
                Object valOrNode = kvPairs[keyIndex + 1];
                if (keyOrNull == null)
                {
                    TrieNode<K, V> newNode =
                        ((TrieNode<K, V>) valOrNode)
                            .mWith(hash, shift + 5, key, value, results);
                    if (newNode != valOrNode)
                    {
                        kvPairs[keyIndex + 1] = newNode;
                    }
                }
                else if (equivKeys(keyOrNull, key))
                {
                    if (!equivVals(valOrNode, value))
                    {
                        kvPairs[keyIndex + 1] = value;
                    }
                }
                else
                {
                    results.keyAdded();
                    TrieNode newNode = resolveCollision(hashCodeFor(keyOrNull),
                                                        keyOrNull,
                                                        valOrNode,
                                                        hash,
                                                        key,
                                                        value);
                    kvPairs[keyIndex] = null;
                    kvPairs[keyIndex + 1] = newNode;
                }
            }
            else
            {
                results.keyAdded();
                int numVals = Integer.bitCount(bitmap);
                if (numVals >= HAMN_CONVERSION_SIZE)
                {
                    return expand(key, value, hashFragment, numVals);
                }
                else if (numVals * 2 < kvPairs.length)
                {
                    System.arraycopy(kvPairs, keyIndex, kvPairs, keyIndex + 2, 2 * numVals - keyIndex);
                    kvPairs[keyIndex] = key;
                    kvPairs[keyIndex + 1] = value;
                    bitmap |= bit;
                }
                else
                {
                    // Give the new array some extra space.
                    kvPairs = cloneExtendAndInsert(kvPairs,
                                                   numVals,
                                                   2 * (numVals + 2),
                                                   keyIndex,
                                                   key,
                                                   value);
                    bitmap |= bit;
                }
            }

            return this;
        }


        private HashArrayMappedNode<K, V> expand(Object key,
                                                 Object value,
                                                 int hashFragment,
                                                 int numVals)
        {
            TrieNode<K, V>[] nodes = new TrieNode[32];
            nodes[hashFragment] = new FlatNode(key, value);

            int j = 0;
            for (int i = 0; i < 32; i++)
            {
                if (isInBitmap(1 << i))
                {
                    if (kvPairs[j] == null)
                    {
                        nodes[i] = (TrieNode<K, V>) kvPairs[j + 1];
                    }
                    else
                    {
                        nodes[i] = new FlatNode(kvPairs[j], kvPairs[j + 1]);
                    }
                    j += 2;
                }
            }
            return new HashArrayMappedNode<>(numVals + 1, nodes);
        }


        @Override
        V get(int hash, int shift, Object key)
        {
            int hashFragment = hashFragment(hash, shift);
            int bit = bitPosition(hashFragment);
            if (!isInBitmap(bit))
            {
                return null;
            }

            int keyIndex = 2 * arrayIndex(bit);
            Object keyOrNull = kvPairs[keyIndex];
            Object valOrNode = kvPairs[keyIndex + 1];
            if (keyOrNull == null)
            {
                return ((TrieNode<K, V>) valOrNode).get(hash, shift + 5, key);
            }
            else if (equivKeys(keyOrNull, key))
            {
                return (V) valOrNode;
            }
            else
            {
                return null;
            }
        }


        @Override
        TrieNode<K, V> without(int hash, int shift, K key)
        {
            int hashFragment = hashFragment(hash, shift);
            int bit = bitPosition(hashFragment);
            if (!isInBitmap(bit))
            {
                return this;
            }

            int keyIndex = 2 * arrayIndex(bit);
            Object keyOrNull = kvPairs[keyIndex];
            Object valOrNode = kvPairs[keyIndex + 1];
            if (keyOrNull == null)
            {
                TrieNode<K, V> newNode =
                    ((TrieNode<K, V>) valOrNode).without(hash, shift + 5, key);
                if (newNode == valOrNode)
                {
                    return this;
                }
                else if (newNode == null)
                {
                    if (bitmap == bit)
                    {
                        return null;
                    }
                    else
                    {
                        return new BitMappedNode<>(bitmap ^ bit,
                                                   cloneAndRemovePair(kvPairs, keyIndex));
                    }
                }
                else
                {
                    return new BitMappedNode<>(bitmap,
                                               cloneAndModify(kvPairs,
                                                              keyIndex + 1,
                                                              newNode));
                }
            }
            else if (equivKeys(keyOrNull, key))
            {
                if (bitmap == bit)
                {
                    return null;
                }
                else
                {
                    return new BitMappedNode<>(bitmap ^ bit,
                                               cloneAndRemovePair(kvPairs, keyIndex));
                }
            }
            else
            {
                return this;
            }
        }


        @Override
        Iterator<Entry<K, V>> iterator()
        {
            return new EntryArrayIterator<>(kvPairs);
        }


        /**
         * The index in the array corresponds to the number of bits "below" this one.
         */
        int arrayIndex(int bit)
        {
            return Integer.bitCount(bitmap & (bit - 1));
        }


        boolean isInBitmap(int bit)
        {
            return (bitmap & bit) != 0;
        }


        /**
         * Used for resolving hashFragment collisions within {@link BitMappedNode}s.
         *
         * Keys with hashes that are exactly the same will be placed into a {@link CollisionNode},
         * while keys with hashes that aren't exactly the same will be nested in a {@link FlatNode}.
         */
        private static <K, V> FlatNode<K, V> resolveCollision(int key1hash,
                                                              K key1,
                                                              V value1,
                                                              int key2hash,
                                                              K key2,
                                                              V value2)
        {
            if (key1hash == key2hash)
            {
                return new CollisionNode<>(key1hash, new Object[]{key1, value1, key2, value2});
            }
            else
            {
                return new FlatNode<>(key1, value1, key2, value2);
            }
        }
    }

    /**
     * {@link HashArrayMappedNode} is the equivalent of Clojure's PersistentHashMap's ArrayNode.
     *
     * This node only stores other nodes as an update speed optimization and contains no key-value pairs.
     */
    static class HashArrayMappedNode<K, V>
        extends TrieNode<K, V>
    {
        /**
         * count refers to the number of non-null nodes in {@link #nodes},
         * not the size of the subtrie.
         */
        private int count;
        final TrieNode<K, V>[] nodes;

        HashArrayMappedNode(int count, TrieNode<K, V>[] nodes)
        {
            this.count = count;
            this.nodes = nodes;
            assert nodes.length == 32;
        }


        @Override
        public TrieNode<K, V> with(int hash,
                                   int shift,
                                   Object key,
                                   Object value,
                                   Results results)
        {
            int index = hashFragment(hash, shift);
            TrieNode<K, V> node = nodes[index];

            if (node == null)
            {
                results.keyAdded();
                TrieNode<K, V> newNode = new FlatNode(key, value);
                return new HashArrayMappedNode<>(count + 1,
                                                 cloneAndModify(nodes,
                                                                index,
                                                                newNode));
            }
            else
            {
                TrieNode<K, V> newNode = node.with(hash, shift + 5, key, value, results);
                if (newNode == node)
                {
                    return this;
                }
                else
                {
                    return new HashArrayMappedNode<>(count,
                                                     cloneAndModify(nodes,
                                                                    index,
                                                                    newNode));
                }
            }
        }


        @Override
        TrieNode<K, V> mWith(int hash,
                             int shift,
                             Object key,
                             Object value,
                             Results results)
        {
            int index = hashFragment(hash, shift);
            TrieNode<K, V> node = nodes[index];

            if (node == null)
            {
                results.keyAdded();
                TrieNode<K, V> newNode = new FlatNode(key, value);
                nodes[index] = newNode;
                count++;
            }
            else
            {
                TrieNode<K, V> newNode = node.mWith(hash, shift + 5, key, value, results);
                if (newNode != node)
                {
                    nodes[index] = newNode;
                }
            }

            return this;
        }


        @Override
        public V get(int hash, int shift, Object key)
        {
            int index = hashFragment(hash, shift);
            TrieNode<K, V> node = nodes[index];
            if (node == null)
            {
                return null;
            }
            else
            {
                return node.get(hash, shift + 5, key);
            }
        }


        @Override
        public TrieNode<K, V> without(int hash, int shift, K key)
        {
            int index = hashFragment(hash, shift);
            TrieNode<K, V> node = nodes[index];
            if (node == null)
            {
                return this;
            }
            else
            {
                TrieNode<K, V> newNode = node.without(hash, shift + 5, key);
                if (newNode == node)
                {
                    return this;
                }
                else if (newNode != null)
                {
                    return new HashArrayMappedNode<>(count,
                                                     cloneAndModify(nodes,
                                                                    index,
                                                                    newNode));
                }
                else
                {
                    if (count <= 8)
                    {
                        return removeAndPack(index);
                    }
                    else
                    {
                        return new HashArrayMappedNode<>(count - 1,
                                                         cloneAndModify(nodes,
                                                                        index,
                                                                        null));
                    }
                }
            }
        }


        @Override
        Iterator<Entry<K, V>> iterator()
        {
            return new Iterator<Entry<K , V>>()
            {
                private final TrieNode<K, V>[] array = nodes;
                private Iterator<Entry<K, V>> childIter;
                private int i;

                private boolean getAndCacheNext()
                {
                    while (i < array.length)
                    {
                        TrieNode<K, V> node = array[i++];
                        if (node != null)
                        {
                            childIter = node.iterator();
                            return true;
                        }
                    }
                    return false;
                }

                @Override
                public boolean hasNext()
                {
                    return childIter != null || getAndCacheNext();
                }

                @Override
                public Entry<K, V> next()
                {
                    if (childIter != null)
                    {
                        Entry<K, V> ret = childIter.next();
                        if (!childIter.hasNext())
                        {
                            childIter = null;
                        }
                        return ret;
                    }
                    else if (getAndCacheNext())
                    {
                        return next();
                    }
                    else
                    {
                        throw new NoSuchElementException();
                    }
                }

                @Override
                public void remove()
                {
                    throw new UnsupportedOperationException();
                }
            };
        }


        /**
         * Removes the node at the specified index before reducing this
         * {@link HashArrayMappedNode} back down to a {@link BitMappedNode}
         */
        private BitMappedNode<K, V> removeAndPack(int index)
        {
            Object[] kvPairs = new Object[2 * (count - 1)];
            int j = 1;
            int bitmap = 0;
            for (int i = 0; i < index; i++)
            {
                if (nodes[i] != null)
                {
                    kvPairs[j] = nodes[i];
                    bitmap |= 1 << i;
                    j += 2;
                }
            }
            for (int i = index + 1; i < nodes.length; i++)
            {
                if (nodes[i] != null)
                {
                    kvPairs[j] = nodes[i];
                    bitmap |= 1 << i;
                    j += 2;
                }
            }
            return new BitMappedNode<>(bitmap, kvPairs);
        }
    }

    /**
     * Iterator for {@link FlatNode}s, {@link CollisionNode}s, and {@link BitMappedNode}s,
     * since they all have nearly the same invariants.
     *
     * A minor optimization could be made for {@link CollisionNode}s similar to that done
     * for {@link HashArrayMappedNode}s, since {@link CollisionNode}s never have child nodes.
     */
    static final class EntryArrayIterator<K, V>
        implements Iterator<Entry<K, V>>
    {
        private final Object[] array;
        private int i = 0;

        // At most one of these is non-null because an array will contain
        // either a key then value pair or a null then a node pair.
        private Iterator<Entry<K, V>> childIterator;
        private Entry<K, V> nextEntry;

        private EntryArrayIterator(Object[] array)
        {
            this.array = array;
        }

        private boolean getAndCacheNext()
        {
            while (i < array.length)
            {
                Object keyOrNull = array[i++];
                Object valOrNode = array[i++];
                if (keyOrNull != null)
                {
                    nextEntry = createEntry((K) keyOrNull, (V) valOrNode);
                    return true;
                }
                else if (valOrNode != null)
                {
                    childIterator = ((TrieNode<K, V>) valOrNode).iterator();
                    return true;
                }
            }
            return false;
        }

        @Override
        public boolean hasNext()
        {
            return childIterator != null || nextEntry != null || getAndCacheNext();
        }

        @Override
        public Entry<K, V> next()
        {
            if (childIterator != null)
            {
                Entry<K, V> ret = childIterator.next();
                if (!childIterator.hasNext())
                {
                   childIterator = null;
                }
                return ret;
            }
            else if (nextEntry != null)
            {
                Entry<K, V> ret = nextEntry;
                nextEntry = null;
                return ret;
            }
            else if (getAndCacheNext())
            {
                return next();
            }
            else
            {
                throw new NoSuchElementException();
            }
        }

        @Override
        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }


    //==========================================================================
    // Utility methods for node classes.

    /**
     * In case key comparison needs to change in the future.
     * @return true if keys are equal (however we define that), false otherwise.
     */
    private static <K> boolean equivKeys(K key1, K key2)
    {
        return key1 == key2 || key1.equals(key2);
    }

    /**
     * In case value comparison needs to change in the future.
     *
     * @return true if the values are equal (however we define that), false otherwise.
     */
    private static <V> boolean equivVals(V val1, V val2)
    {
        return val1 == val2;
    }

    /**
     * Short, generified wrapper for AbstractMap.SimpleEntry.
     */
    private static <K, V> Entry<K, V> createEntry(K key,
                                                      V value)
    {
        return new SimpleEntry<>(key, value);
    }

    private static Object[] cloneAndExtend(Object[] array,
                                           Object key,
                                           Object value)
    {
        int len = array.length;
        Object[] newArray = Arrays.copyOf(array, len + 2, Object[].class);
        newArray[len] = key;
        newArray[len + 1] = value;
        return newArray;
    }

    private static Object[] cloneExtendAndInsert(Object[] array,
                                                 int oldNumVals,
                                                 int newLen,
                                                 int keyIndex,
                                                 Object key,
                                                 Object value)
    {
        Object[] newArray = Arrays.copyOf(array, newLen, Object[].class);
        newArray[keyIndex] = key;
        newArray[keyIndex + 1] = value;
        System.arraycopy(array, keyIndex, newArray, keyIndex + 2, 2 * oldNumVals - keyIndex);
        return newArray;
    }

    private static <K, V> TrieNode<K, V>[] cloneAndModify(TrieNode<K, V>[] array,
                                                          int i,
                                                          TrieNode<K, V> a)
    {
        TrieNode<K, V>[] clone = array.clone();
        clone[i] = a;
        return clone;
    }

    private static Object[] cloneAndModify(Object[] array, int i, Object a)
    {
        Object[] clone = array.clone();
        clone[i] = a;
        return clone;
    }

    private static Object[] cloneAndModify(Object[] array,
                                           int i,
                                           Object a,
                                           int j,
                                           Object b)
    {
        Object[] clone = array.clone();
        clone[i] = a;
        clone[j] = b;
        return clone;
    }

    private static Object[] cloneAndRemovePair(Object[] array, int keyIndex)
    {
        Object[] clone = Arrays.copyOf(array, array.length - 2, Object[].class);
        System.arraycopy(array, keyIndex + 2, clone, keyIndex, clone.length - keyIndex);
        return clone;
    }
}
