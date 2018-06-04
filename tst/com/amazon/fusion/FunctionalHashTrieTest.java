package com.amazon.fusion;

import org.junit.Test;

import java.util.AbstractMap;
import java.util.ArrayList;
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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
        final String key;

        CustomKey(int hash, String key)
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
        Object[] values = {"key1", "foo", "key2", "bar"};
        TrieNode collisionNode = new CollisionNode(2, values);

        Box addedLeaf = new Box(null);

        TrieNode withNewNonCollidingKey =
            collisionNode.with(1, 0, "key3", "baz", addedLeaf);

        // We expect a new leaf to have been added.
        assertTrue(addedLeaf.value != null);
        // We expect the collision node to have been pushed down a level.
        assertTrue(withNewNonCollidingKey instanceof FlatNode);
        assertEquals("foo", withNewNonCollidingKey.get(2, 0, "key1"));
        assertEquals("bar", withNewNonCollidingKey.get(2, 0, "key2"));
        assertEquals("baz", withNewNonCollidingKey.get(1, 0, "key3"));

        TrieNode withCollidingKey =
            withNewNonCollidingKey.with(2, 0, "key4", "biz", addedLeaf);
        assertEquals("biz", withCollidingKey.get(2, 0, "key4"));

        TrieNode withoutKey1 = withCollidingKey.without(2, 0, "key1");
        assertEquals(null, withoutKey1.get(2, 0, "key1"));
        assertEquals("bar", withoutKey1.get(2, 0, "key2"));
        assertEquals("baz", withoutKey1.get(1, 0, "key3"));
        assertEquals("biz", withoutKey1.get(2, 0, "key4"));

        TrieNode without1And2 = withoutKey1.without(2, 0, "key2");
        assertEquals(null, without1And2.get(2, 0, "key1"));
        assertEquals(null, without1And2.get(2, 0, "key2"));
        assertEquals("baz", without1And2.get(1, 0, "key3"));
        assertEquals("biz", withoutKey1.get(2, 0, "key4"));

        TrieNode without123 = without1And2.without(1, 0, "key3");
        assertEquals(null, without123.get(2, 0, "key1"));
        assertEquals(null, without123.get(2, 0, "key2"));
        assertEquals(null, without123.get(1, 0, "key3"));
        assertEquals("biz", without123.get(2, 0, "key4"));

        TrieNode empty = without123.without(2, 0, "key4");
        assertTrue(empty == null);
    }

    @Test
    public void checkFlatNodeExpansion()
    {
        List<Object> values = new LinkedList<>();
        for (int i = 0; i < 7; i++)
        {
            CustomKey key = new CustomKey(i, Integer.toString(i));
            values.add(key);
            values.add(new Object());
        }

        TrieNode flatNode = new FlatNode(values.toArray());

        Box addedLeaf = new Box(null);
        Map.Entry eighth = new AbstractMap.SimpleEntry<>(new CustomKey(8, "8"),
                                                         new Object());
        values.add(eighth);

        TrieNode stillFlat = flatNode.with(8, 0, eighth.getKey(), eighth.getValue(), addedLeaf);
        assertTrue(stillFlat instanceof FlatNode);

        addedLeaf = new Box(null);
        Map.Entry ninth = new AbstractMap.SimpleEntry<>(new CustomKey(9, "9"),
                                                        new Object());
        values.add(ninth);

        TrieNode nowBitMap = stillFlat.with(9, 0, ninth.getKey(), ninth.getValue(), addedLeaf);
        assertTrue(nowBitMap instanceof BitMappedNode);
        assertEquals(9, countNodeSize(nowBitMap));

        BitMappedNode bm = (BitMappedNode) nowBitMap;
        for (int i = 0; i < 9; i++)
        {
            assertEquals(values.get(i), bm.kvPairs[i]);
            assertNotNull(bm.kvPairs[i + 1]);
        }
    }

    @Test
    public void checkFlatNodeExpansionWithCollisionNode()
    {
        Object[] values = {"key1", "foo", "key2", "bar"};
        TrieNode trieNode = new CollisionNode(0, values);

        List<CustomKey> keys = new ArrayList<>(8);

        Box addedLeaf = new Box(null);

        for (int i = 1; i < 9; i++)
        {
            CustomKey key = new CustomKey(i, Integer.toString(i));
            keys.add(key);
            trieNode = trieNode.with(i, 0, key, new Object(), addedLeaf);
        }

        // 10 because the nested collision node has 2 elements.
        assertEquals(10, countNodeSize(trieNode));

        assertTrue(trieNode instanceof BitMappedNode);
        BitMappedNode bitMappedNode = (BitMappedNode) trieNode;
        for (int i = 1; i < 9; i++)
        {
            assertEquals(keys.get(i - 1), bitMappedNode.kvPairs[i * 2]);
        }

        CollisionNode collisionNode = (CollisionNode) bitMappedNode.kvPairs[1];
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
        trieNode = trieNode.with(0, 0, foo, new Object(), dummy);
        trieNode = trieNode.with(0, 0, bar, new Object(), dummy);

        Object shouldBeNull = ((BitMappedNode) trieNode).kvPairs[0];
        assertNull(shouldBeNull);

        Object shouldBeNode = ((BitMappedNode) trieNode).kvPairs[1];
        assertTrue(shouldBeNode instanceof CollisionNode);
        assertEquals(2, countNodeSize(trieNode));
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
        TrieNode[] subNodes = new TrieNode[32];
        Object[] flatNodeValues = new Object[]{"key1", "val1"};
        subNodes[10] = new FlatNode(flatNodeValues);

        // A HAM Node with only 1 value will never happen, but does test coverage well.
        TrieNode node = new HashArrayMappedNode(1, subNodes);
        TrieNode newNode = node.without(30, 0, "notInHAMNode");
        assertTrue(newNode == node);

        TrieNode stillSameNode = node.without(10, 0, "notInFlatNode");
        assertTrue(stillSameNode == node);

        TrieNode sameAfterWith = node.with(10, 0, "key1", "val1", new Box(null));
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

    @Test
    public void testCorrectShifting()
    {
        Box ignored = new Box(null);
        TrieNode nested = BitMappedNode.EMPTY.with(0x20, 5, new CustomKey(0x20, "foo"), 1, ignored);
        // 0x20 = 0b00001 00000 -> the second slot in a bitmap with a node at shift 5.
        assertEquals(0b010, ((BitMappedNode) nested).bitmap);

        TrieNode top = new BitMappedNode(0b1, new Object[]{null, nested});
        // a bitmap of 0b1 to indicate that the hashFragment corresponding to 0b00000 is occupied.
        // 0b00000 is occupied because a hash of 0x20 with a shift of 0 would be nested at 0b00000

        top = top.with(0x40, 0, new CustomKey(0x40, "bar"), 2, ignored);
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
            for (int i = 1; i < 8; i++)
            {
                customFHT = customFHT.with(new CustomKey(i << shift, UUID.randomUUID().toString()), UUID.randomUUID().toString());
            }
        }
        // The last level has to be more custom built since the only possible hash keys are going to be 00, 01, 10, and 11.
        // If we iterated over i for keys then every 4th key would end up in the CollisionNode at the bottom.
        for (int i = 0; i < 8; i++)
        {
            customFHT = customFHT.with(new CustomKey(1 << 30, UUID.randomUUID().toString()), UUID.randomUUID().toString());
        }

        assertEquals(52, customFHT.size());
        assertEquals("A", customFHT.get(new CustomKey(0, "foo")));
        assertEquals("B", customFHT.get(new CustomKey(0, "bar")));

        TrieNode node = customFHT.getRoot();                        // Shift 0:     BMNode [↓, UUIDs...]
        for (int shift = 0; shift <= 30; shift += 5)                // Shift 5:     BMNode [↓, UUIDs...]
        {                                                           // Shift 10:    BMNode [↓, UUIDs...]
            assertTrue(node instanceof BitMappedNode);              // Shift 15:    BMNode [↓, UUIDs...]
            node = (TrieNode) ((BitMappedNode) node).kvPairs[1];     // Shift 20:    BMNode [↓, UUIDs...]
        }                                                           // Shift 25:    BMNode [↓, UUIDs...]
        assertTrue(node instanceof CollisionNode);                  // Shift 30:    BMNode [↓, UUIDs...]
                                                                    // Shift NA:    CollisionNode["foo", "A", "bar", "B"]
        Object[] values = ((CollisionNode) node).kvPairs;
        assertEquals("A", values[1]);
        assertEquals("B", values[3]);


        // Now let's verify that accesses don't shift over 30. There's an assert in hashFragment()
        customFHT = customFHT.with(new CustomKey(4 << 30, "baz"), "C"); // 4 << 30 == 0 << 30 :)

        assertEquals(53, customFHT.size());
        assertEquals("A", customFHT.get(new CustomKey(0, "foo")));
        assertEquals("B", customFHT.get(new CustomKey(0, "bar")));
        assertEquals("C", customFHT.get(new CustomKey(0, "baz")));

        node = customFHT.getRoot();                                 // Shift 0:     BMNode [↓, UUIDs...]
        for (int shift = 0; shift <= 30; shift += 5)                // Shift 5:     BMNode [↓, UUIDs...]
        {                                                           // Shift 10:    BMNode [↓, UUIDs...]
            assertTrue(node instanceof BitMappedNode);              // Shift 15:    BMNode [↓, UUIDs...]
            node = (TrieNode) ((BitMappedNode) node).kvPairs[1];     // Shift 20:    BMNode [↓, UUIDs...]
        }                                                           // Shift 25:    BMNode [↓, UUIDs...]
        assertTrue(node instanceof CollisionNode);                  // Shift 30:    BMNode [↓, UUIDs...]
                                                                    // Shift NA:    CollisionNode["foo", "A", "bar", "B", "baz", "C"]
        values = ((CollisionNode) node).kvPairs;
        assertEquals("A", values[1]);
        assertEquals("B", values[3]);
        assertEquals("C", values[5]);
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
