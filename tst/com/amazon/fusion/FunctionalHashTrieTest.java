package com.amazon.fusion;

import org.junit.Test;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static com.amazon.fusion.FunctionalHashTrie.EMPTY;
import static com.amazon.fusion.FunctionalHashTrie.TrieNode;
import static com.amazon.fusion.FunctionalHashTrie.FlatNode;
import static com.amazon.fusion.FunctionalHashTrie.CollisionNode;
import static com.amazon.fusion.FunctionalHashTrie.BitMappedNode;
import static com.amazon.fusion.FunctionalHashTrie.HashArrayMappedNode;
import static com.amazon.fusion.FunctionalHashTrie.hashCodeFor;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class FunctionalHashTrieTest
{
    /**
     * This class allows us to easily for hash collisions for different keys.
     */
    private static class CustomKey
    {
        final int hash;
        final Object key;

        CustomKey(int hash, Object key)
        {
            this.hash = hash;
            this.key = key;
        }

        @Override
        public String toString()
        {
            return "Hash: " + hash + " Key: " + key;
        }

        @Override
        public int hashCode()
        {
            return hash;
        }

        @Override
        public boolean equals(Object obj)
        {
            if (obj instanceof CustomKey)
            {
                return key.equals(((CustomKey) obj).key);
            }
            return false;
        }
    }

    private Map<Object, Object> baselineMap;
    private FunctionalHashTrie<Object, Object> fht;

    public void setup(int size)
    {
        baselineMap = new HashMap<>();
        for (int i = 0; i < size; i++)
        {
            baselineMap.put(new Object(), new Object());
        }
        fht = FunctionalHashTrie.create(baselineMap);
    }

    private void doRemoval()
    {
        Object[] keys = baselineMap.keySet().toArray();
        Set<Object> keysToRemove = new LinkedHashSet<>(keys.length / 2);
        for (int i = 0; i < keys.length / 2; i++)
        {
            keysToRemove.add(keys[i]);
        }

        baselineMap.keySet().removeAll(keysToRemove);

        for (Object key : keysToRemove)
        {
            fht = fht.without(key);
        }
    }

    private void doAddition()
    {
        int toAdd = baselineMap.size();
        for (int i = 0; i < toAdd; i++)
        {
            Map.Entry entry = new AbstractMap.SimpleEntry<>(new Object(), new Object());

            baselineMap.put(entry.getKey(), entry.getValue());
            fht = fht.with(entry.getKey(), entry.getValue());
        }
    }

    private void checkSizing()
    {
        assertEquals(baselineMap.size(), fht.size());
    }

    private void checkIterators()
    {
        Set<Object> keySet = fht.keySet();
        Set<Object> baselineSet = baselineMap.keySet();
        assertEquals(baselineSet, keySet);

        Set<Object> values = new HashSet<>(fht.values());
        Set<Object> baselineValues = new HashSet<>(baselineMap.values());
        assertEquals(baselineValues, values);

        Set<Map.Entry<Object, Object>> entrySet = fht.entrySet();
        Set<Map.Entry<Object, Object>> baselineEntrySet = baselineMap.entrySet();
        assertEquals(baselineEntrySet, entrySet);
    }

    private void compareEntries()
    {
        for (Map.Entry entry : baselineMap.entrySet())
        {
            Object baselineValue = entry.getValue();
            assertEquals(baselineValue, fht.get(entry.getKey()));
        }
    }

    private void compareWithBaseline()
    {
        checkSizing();
        checkIterators();
        compareEntries();
        assertEquals(baselineMap, fht);
        assertEquals(baselineMap.hashCode(), fht.hashCode());
    }

    private void performTests()
    {
        compareWithBaseline();
        doRemoval();
        compareWithBaseline();
        doAddition();
        compareWithBaseline();
    }

    @Test
    public void checkEmpty()
    {
        setup(0);
        assertTrue(fht.isEmpty());
        assertEquals(baselineMap.toString(), fht.toString());
        performTests();

        FunctionalHashTrie without = fht.without("anything");
        assertTrue(fht == without);
        assertTrue(fht == EMPTY);
    }

    @Test
    public void checkSingle()
    {
        setup(1);
        assertEquals(baselineMap.toString(), fht.toString());
        performTests();
    }

    @Test
    public void checkSmall()
    {
        setup(10);
        performTests();
    }

    @Test
    public void checkVeryLarge()
    {
        setup(10000);
        performTests();
    }

    @Test
    public void checkVeryVeryLarge()
    {
        setup(1000000);
        performTests();
    }

    @Test
    public void checkCollisionNodeBehavior()
    {
        CustomKey key1 = new CustomKey(2, "key1");
        CustomKey key2 = new CustomKey(2, "key2");
        Object[] values = { key1, "foo", key2, "bar" };
        TrieNode collisionNode = new CollisionNode(hashCodeFor(2), values);

        Box addedLeaf = new Box(null);

        CustomKey key3 = new CustomKey(1, "key3");
        TrieNode withNewNonCollidingKey =
            collisionNode.with(hashCodeFor(1), 0, key3, "baz", addedLeaf);

        // We expect a new leaf to have been added.
        assertTrue(addedLeaf.value != null);
        // We expect the collision node to have been pushed down a level.
        assertTrue(withNewNonCollidingKey instanceof FlatNode);
        assertEquals("foo", withNewNonCollidingKey.get(hashCodeFor(2), 0, key1));
        assertEquals("bar", withNewNonCollidingKey.get(hashCodeFor(2), 0, key2));
        assertEquals("baz", withNewNonCollidingKey.get(hashCodeFor(1), 0, key3));

        CustomKey key4 = new CustomKey(2, "key4");
        TrieNode withCollidingKey =
            withNewNonCollidingKey.with(hashCodeFor(2), 0, key4, "biz", addedLeaf);
        assertEquals("biz", withCollidingKey.get(hashCodeFor(2), 0, key4));

        TrieNode withoutKey1 = withCollidingKey.without(hashCodeFor(2), 0, key1);
        assertEquals(null, withoutKey1.get(hashCodeFor(2), 0, key1));
        assertEquals("bar", withoutKey1.get(hashCodeFor(2), 0, key2));
        assertEquals("baz", withoutKey1.get(hashCodeFor(1), 0, key3));
        assertEquals("biz", withoutKey1.get(hashCodeFor(2), 0, key4));

        TrieNode without1And2 = withoutKey1.without(hashCodeFor(2), 0, key2);
        assertEquals(null, without1And2.get(hashCodeFor(2), 0, key1));
        assertEquals(null, without1And2.get(hashCodeFor(2), 0, key2));
        assertEquals("baz", without1And2.get(hashCodeFor(1), 0, key3));
        assertEquals("biz", withoutKey1.get(hashCodeFor(2), 0, key4));

        TrieNode without123 = without1And2.without(hashCodeFor(1), 0, key3);
        assertEquals(null, without123.get(hashCodeFor(2), 0, key1));
        assertEquals(null, without123.get(hashCodeFor(2), 0, key2));
        assertEquals(null, without123.get(hashCodeFor(1), 0, key3));
        assertEquals("biz", without123.get(hashCodeFor(2), 0, key4));

        TrieNode empty = without123.without(hashCodeFor(2), 0, key4);
        assertTrue(empty == null);
    }

    @Test
    public void checkFlatNodeExpansion()
    {
        List<Map.Entry> values = new LinkedList<>();
        for (int i = 0; i < 9; i++)
        {
            CustomKey key = new CustomKey(i, Integer.toString(i));
            Map.Entry entry = new AbstractMap.SimpleEntry<>(key, new Object());
            values.add(entry);
        }

        TrieNode flatNode = new FlatNode(new Object[0]);

        Box addedLeaf = new Box(null);
        // A flat node should hold eight entries before expanding.
        for (int i = 0; i < 8; i++)
        {
            Map.Entry entry = values.get(i);
            flatNode = flatNode.with(hashCodeFor(entry.getKey()),
                                     0,
                                     entry.getKey(),
                                     entry.getValue(),
                                     addedLeaf);
            assertTrue(flatNode instanceof FlatNode);
        }

        Map.Entry ninth = values.get(8);
        TrieNode nowBitMap = flatNode.with(hashCodeFor(ninth.getKey()),
                                           0,
                                           ninth.getKey(),
                                           ninth.getValue(),
                                           addedLeaf);
        assertTrue(nowBitMap instanceof BitMappedNode);
        assertEquals(9, countNodeSize(nowBitMap));

        BitMappedNode bm = (BitMappedNode) nowBitMap;
        Object[] kvPairs = bm.kvPairs;
        // BMNodes will allocate some extra space during expansion since expansion uses the mWith() method.
        assertEquals(values.size() * 2 + 2, kvPairs.length);
        for (int i = 0; i < 9; i++)
        {
            Map.Entry entry = values.get(i);
            boolean found = false;
            for (int k = 0; k < kvPairs.length; k += 2)
            {
                if (entry.getKey().equals(kvPairs[k]))
                {
                    assertFalse(found);
                    found = true;
                    assertSame(entry.getValue(), kvPairs[k + 1]);
                }
            }
        }
    }

    @Test
    public void checkFlatNodeExpansionWithCollisionNode()
    {
        Object[] values = {"key1", "foo", "key2", "bar"};
        TrieNode trieNode = new CollisionNode(hashCodeFor(0), values);

        List<CustomKey> keys = new ArrayList<>(8);

        Box addedLeaf = new Box(null);

        for (int i = 1; i < 9; i++)
        {
            CustomKey key = new CustomKey(i, Integer.toString(i));
            keys.add(key);
            trieNode = trieNode.with(hashCodeFor(i), 0, key, new Object(), addedLeaf);
        }

        // 10 because the nested collision node has 2 elements.
        assertEquals(10, countNodeSize(trieNode));

        assertTrue(trieNode instanceof BitMappedNode);
        BitMappedNode bitMappedNode = (BitMappedNode) trieNode;
        Object[] kvPairs = bitMappedNode.kvPairs;
        List<Object> kvList = Arrays.asList(kvPairs);
        for (Object key : keys)
        {
            assertTrue(kvList.contains(key));
        }

        int collisionNodeIndex = -1;
        for (int i = 0; i < kvPairs.length; i++)
        {
            if (kvPairs[i] instanceof CollisionNode)
            {
                collisionNodeIndex = i;
            }
        }

        assertNotEquals("CollisionNode does not exist within BitMappedNode", -1, collisionNodeIndex);

        CollisionNode collisionNode = (CollisionNode) bitMappedNode.kvPairs[collisionNodeIndex];
        assertEquals("key1", collisionNode.kvPairs[0]);
        assertEquals("foo", collisionNode.kvPairs[1]);
        assertEquals("key2", collisionNode.kvPairs[2]);
        assertEquals("bar", collisionNode.kvPairs[3]);
    }

    @Test
    public void checkCollisionNodeCreation()
    {
        Box dummy = new Box(null);
        CustomKey foo = new CustomKey(0, "foo");
        CustomKey bar = new CustomKey(0, "bar");
        TrieNode trieNode = BitMappedNode.EMPTY;
        trieNode = trieNode.with(hashCodeFor(0), 0, foo, new Object(), dummy);
        trieNode = trieNode.with(hashCodeFor(0), 0, bar, new Object(), dummy);

        Object shouldBeNull = ((BitMappedNode) trieNode).kvPairs[0];
        assertNull(shouldBeNull);

        Object shouldBeNode = ((BitMappedNode) trieNode).kvPairs[1];
        assertTrue(shouldBeNode instanceof CollisionNode);
        assertEquals(2, countNodeSize(trieNode));
        // This only works because values are stored into collision nodes in order of insertion...
        assertEquals(foo, ((CollisionNode) shouldBeNode).kvPairs[0]);
        assertEquals(bar, ((CollisionNode) shouldBeNode).kvPairs[2]);
    }

    @Test
    public void checkBitMappedNodeExpansionAndArrayNodeCompression()
    {
        Box dummy = new Box(null);
        Object[] keys = new Object[17];
        TrieNode trieNode = BitMappedNode.EMPTY;
        for (int i = 0; i < 16; i++)
        {
            Object key = new Object();
            keys[i] = key;
            trieNode = trieNode.with(i, 0, key, new Object(), dummy);
        }

        assertTrue(trieNode instanceof BitMappedNode);
        assertEquals(16, countNodeSize(trieNode));

        Object overConversionLimitKey = new Object();
        keys[16] = overConversionLimitKey;
        trieNode = trieNode.with(16, 0, overConversionLimitKey, new Object(), dummy);
        assertTrue(trieNode instanceof HashArrayMappedNode);
        assertEquals(17, countNodeSize(trieNode));
        for (int i = 0; i < 17; i++)
        {
            assertNotNull("Key: " + keys[i] + " at index " + i + " was not found.", trieNode.get(i, 0, keys[i]));
        }

        for (int i = 0; i < 9; i++)
        {
            Object key = keys[i];
            keys[i] = null;
            trieNode = trieNode.without(i, 0, key);
        }
        assertTrue(trieNode instanceof HashArrayMappedNode);
        assertEquals(8, countNodeSize(trieNode));
        for (int i = 9; i < 17; i++)
        {
            assertNotNull("Key: " + keys[i] + " at index " + i + " was not found.", trieNode.get(i, 0, keys[i]));
        }

        trieNode = trieNode.without(9, 0, keys[9]);
        keys[9] = null;

        assertTrue(trieNode instanceof BitMappedNode);
        assertEquals(7, countNodeSize(trieNode));

        for (int i = 10; i < 17; i++)
        {
            assertNotNull("Key: " + keys[i] + " at index " + i + " was not found.", trieNode.get(i, 0, keys[i]));
        }
    }

    @Test
    public void checkHashArrayMappedNode()
    {
        Box added = new Box(null);
        TrieNode node = new FlatNode(new Object[0]);

        for (int i = 0; i < 16; i++)
        {
            CustomKey key = new CustomKey(i, new Object());
            Object value = new Object();
            node = node.with(hashCodeFor(key), 0, key, value, added);
        }
        CustomKey checkKey = new CustomKey(16, "foo");
        String checkVal = "bar";
        node = node.with(hashCodeFor(checkKey), 0, checkKey, checkVal, added);

        assertTrue(node instanceof HashArrayMappedNode);
        assertEquals(17, countNodeSize(node));

        TrieNode newNode = node.without(hashCodeFor(30), 0, "notInHAMNode");
        assertTrue(newNode == node);

        TrieNode stillSameNode = node.without(hashCodeFor(10), 0, "notInFlatNode");
        assertTrue(stillSameNode == node);

        TrieNode sameAfterWith = node.with(hashCodeFor(checkKey), 0, checkKey, checkVal, new Box(null));
        assertTrue(sameAfterWith == node);
    }

    @Test
    public void checkNullKeys()
    {
        setup(1);

        final String failureMessage = "Should have raised a NullPointerException";
        try
        {
            fht.with(null, "foo");
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with("foo", null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with(null, null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.without(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.get(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.containsKey(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.containsValue(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }
    }

    @Test
    public void keysNotInBitMap()
    {
        TrieNode node = BitMappedNode.EMPTY;
        TrieNode stillSame = node.without(0, 0, new Object());
        assertTrue(node == stillSame);
        assertEquals(null, node.get(0, 0, new Object()));
    }

    /**
     * This test verifies that we shift along the hash value correctly when going "down"
     * a level in the trie. An early, pre-release {@link FunctionalHashTrie} impl had
     * a bug where one of the {@code shift + 5} code statements was just {@code shift}.
     *
     * What this test does is that it creates a two level trie.
     * The first level nests the second level in the first slot.
     * The second level contains a value at the second slot.
     *
     * Then, we try to add in a new value which should go into the third slot of the second level.
     * If the shifting along the hash did not get added, it would be placed in the first slot of the second level.
     */
    @Test
    public void testCorrectShifting()
    {
        Box ignored = new Box(null);
        TrieNode nested = BitMappedNode.EMPTY.with(0x20, 5, new CustomKey(0, "redherring"), 1, ignored);
        // 0x20 = 0b00001 00000 -> the second slot in a bitmap with a node at shift 5.
        assertEquals(0b010, ((BitMappedNode) nested).bitmap);

        TrieNode top = new BitMappedNode(0b1, new Object[]{null, nested});
        // a bitmap of 0b1 to indicate that the hashFragment corresponding to 0b00000 is occupied.
        // 0b00000 is occupied because a hash of 0x20 with a shift of 0 would be nested at 0b00000

        top = top.with(0x40, 0, new CustomKey(4, "ignoreme"), 2, ignored);
        // 0x40 = 0b00010 00000 -> the first slot (0b00000) in a bitmap with a node at shift 0.
        // Since that spot is already occupied by a node, we'll insert it in
        // the third slot (0b00010) in the nested node with shift 5.

        Object[] topVals = ((BitMappedNode) top).kvPairs;
        assertEquals(2, topVals.length);
        assertEquals(0b1, ((BitMappedNode) top).bitmap);

        BitMappedNode newNested = (BitMappedNode) topVals[1];
        assertEquals(0b110, newNested.bitmap);
        // If the "bar" key used a shift of 0 when added to a nested node
        // (like in a previous buggy impl) then the hashFragment would have
        // been 0b00000 and the bitmap would have been 0b011

        Object[] newNestedVals = newNested.kvPairs;
        assertEquals(4, newNestedVals.length);
        assertEquals(1, newNestedVals[1]);
        assertEquals(2, newNestedVals[3]);
    }

    /**
     * This constructs a trie such that there are collisions on input hashcode zero,
     * and that the collisions are pushed to the lowest possible node of the trie.
     *
     * The test verifies that at the very lowest level, we will have {@link CollisionNode}s
     * which contain all values with direct hash collisions and that we never fail
     * the {@code shift <= 30} assertion within {@link TrieNode#hashFragment(int, int)}.
     */
    @Test
    public void testDeepShifts()
    {
        FunctionalHashTrie<CustomKey, String> customFHT = EMPTY;

        // These two will be in the collision node at the bottom.
        customFHT = customFHT.with(new CustomKey(0, "foo"), "A");
        customFHT = customFHT.with(new CustomKey(0, "bar"), "B");

        // For each level, push in enough nodes s.t. we convert from a FlatNode to a BitMappedNode
        for (int shift = 0; shift <= 25; shift += 5)
        {
            // Spread out hash keys so that only "A" and "B" are in the CollisionNode at the bottom.
            for (int i = 1; i < 32; i++)
            {
                customFHT = customFHT.with(new CustomKey(i << shift, UUID.randomUUID().toString()), UUID.randomUUID().toString());
            }
        }
        // The last level has to be more custom built since the only possible hash keys are going to be 00, 01, 10, and 11.
        // The last level will also only ever be a BitMappedNode, full of collision nodes at the bottom.
        // If we iterated over i for keys then every 4th key would end up in the CollisionNode at the bottom.
        for (int i = 0; i < 3; i++)
        {
            for (int j = 1; j < 4; j++)
            {
                customFHT = customFHT.with(new CustomKey(j << 30, UUID.randomUUID().toString()), UUID.randomUUID().toString());
            }
        }

        assertEquals(197, customFHT.size());
        assertEquals("A", customFHT.get(new CustomKey(0, "foo")));
        assertEquals("B", customFHT.get(new CustomKey(0, "bar")));

        int shuffledHash = hashCodeFor(0);                                 // Note, due to shuffling it's not necessarily at the first node of the tree.
        TrieNode node = customFHT.getRoot();                                    // Shift 0:     HAMNode [↓, UUIDs...]
        for (int shift = 0; shift <= 25; shift += 5)                            // Shift 5:     HAMNode [↓, UUIDs...]
        {                                                                       // Shift 10:    HAMNode [↓, UUIDs...]
            assertTrue(node instanceof HashArrayMappedNode);                    // Shift 15:    HAMNode [↓, UUIDs...]
            int nestedIndex = (shuffledHash >>> shift) & 0x1f;                  // Shift 20:    HAMNode [↓, UUIDs...]
            node = ((HashArrayMappedNode) node).nodes[nestedIndex];             // Shift 25:    HAMNode [↓, UUIDs...]
        }                                                                       // Shift 30:    BMNode [↓, UUIDs...]
        assertTrue(node instanceof BitMappedNode);                              // Shift NA:    CollisionNode["foo", "A", "bar", "B"]

        // this abuses the fact that a "full" bottom level BMNode won't require bit counting to index into the array.
        int bitMappedNodeIndex = ((shuffledHash >>> 30) & 0x1f) * 2 + 1;
        node = (TrieNode) ((BitMappedNode) node).kvPairs[bitMappedNodeIndex];
        assertTrue(node instanceof CollisionNode);

        List<Object> values = Arrays.asList(((CollisionNode) node).kvPairs);
        assertTrue(values.contains("A"));
        assertTrue(values.contains("B"));


        // Now let's verify that accesses don't shift over 30. There's an assert in hashFragment()
        customFHT = customFHT.with(new CustomKey(4 << 30, "baz"), "C"); // 4 << 30 == 0 << 30 (for 32 bit values)

        assertEquals(198, customFHT.size());
        assertEquals("A", customFHT.get(new CustomKey(0, "foo")));
        assertEquals("B", customFHT.get(new CustomKey(0, "bar")));
        assertEquals("C", customFHT.get(new CustomKey(0, "baz")));

        TrieNode newNode = customFHT.getRoot();                                 // Shift 0:     HAMNode [↓, UUIDs...]
        for (int shift = 0; shift <= 25; shift += 5)                            // Shift 5:     HAMNode [↓, UUIDs...]
        {                                                                       // Shift 10:    HAMNode [↓, UUIDs...]
            assertTrue(newNode instanceof HashArrayMappedNode);                 // Shift 15:    HAMNode [↓, UUIDs...]
            int nestedIndex = (shuffledHash >>> shift) & 0x1f;                  // Shift 20:    HAMNode [↓, UUIDs...]
            newNode = ((HashArrayMappedNode) newNode).nodes[nestedIndex];       // Shift 25:    HAMNode [↓, UUIDs...]
        }                                                                       // Shift 30:    BMNode [↓, UUIDs...]
        assertTrue(newNode instanceof BitMappedNode);                           // Shift NA:    CollisionNode["foo", "A", "bar", "B", "baz", "C"]

        // this abuses the fact that a "full" bottom level BMNode won't require bit counting to index into the array.
        bitMappedNodeIndex = ((shuffledHash >>> 30) & 0x1f) * 2 + 1;
        newNode = (TrieNode) ((BitMappedNode) newNode).kvPairs[bitMappedNodeIndex];
        assertTrue(newNode instanceof CollisionNode);

        List<Object> newValues = Arrays.asList(((CollisionNode) newNode).kvPairs);
        assertTrue(newValues.contains("A"));
        assertTrue(newValues.contains("B"));
        assertTrue(newValues.contains("C"));
    }

    @Test
    public void compareMutableCreateVsSequential()
    {
        setup(10000);

        FunctionalHashTrie seq = EMPTY;
        for (Map.Entry e : fht.entrySet())
        {
            seq = seq.with(e.getKey(), e.getValue());
        }

        assertEquals(fht.size(), seq.size());
        for (Map.Entry e : fht.entrySet())
        {
            Object key = e.getKey();
            assertEquals(fht.get(key), seq.get(key));
        }
    }

    private int countNodeSize(TrieNode node)
    {
        Iterator iter = node.iterator();
        int size = 0;
        while (iter.hasNext())
        {
            iter.next();
            size++;
        }
        return size;
    }
}
