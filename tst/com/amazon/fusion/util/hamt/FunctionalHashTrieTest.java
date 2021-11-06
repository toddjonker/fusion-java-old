// Copyright (c) 2018-2021 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util.hamt;

import static com.amazon.fusion.util.hamt.FunctionalHashTrie.empty;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromArrays;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromEntries;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromMap;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromSelectedKeys;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.amazon.fusion.util.hamt.FunctionalHashTrie.Changes;
import java.util.AbstractMap.SimpleEntry;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.junit.Test;

public class FunctionalHashTrieTest
{
    private HashMap<Object, Object>            baselineMap;
    private FunctionalHashTrie<Object, Object> fht;

    public void setup(int size)
    {
        baselineMap = new HashMap<>();
        for (int i = 0; i < size; i++)
        {
            baselineMap.put(new Object(), new Object());
        }
        fht = fromMap(baselineMap);
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
            FunctionalHashTrie<Object, Object> newHash = fht.withoutKey(key);
            assertNotEquals(fht, newHash);
            fht = newHash;
        }
    }

    private void doAddition()
    {
        int toAdd = baselineMap.size();
        for (int i = 0; i < toAdd; i++)
        {
            Object key   = new Object();
            Object value = new Object();

            baselineMap.put(key, value);
            FunctionalHashTrie<Object, Object> newHash = fht.with1(key, value);
            assertNotEquals(fht, newHash);
            fht = newHash;
        }
    }

    private void checkSizing()
    {
        assertEquals(baselineMap.size(), fht.size());
    }

    private void checkKeys()
    {
        Set<Object> keySet = fht.keySet();
        Set<Object> baselineSet = baselineMap.keySet();
        assertEquals(baselineSet, keySet);
    }

    private void compareEntries()
    {
        Map<Object, Object> remainder = (Map<Object, Object>) baselineMap.clone();

        for (Map.Entry<Object, Object> entry : fht)
        {
            assertEquals(entry.getValue(), remainder.remove(entry.getKey()));
        }

        assertEquals("elements remaining", 0, remainder.size());
    }

    private void compareWithBaseline()
    {
        checkSizing();
        checkKeys();
        compareEntries();
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
        assertSame(empty(), fht);
        performTests();

        FunctionalHashTrie without = fht.withoutKey("anything");
        assertSame(without, fht);
        assertSame(empty(), fht);
    }

    @Test
    public void checkSingle()
    {
        setup(1);
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
    public void checkNullKeys()
    {
        setup(1);

        final String failureMessage = "Should have raised a NullPointerException";
        try
        {
            fht.with1(null, "foo");
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with1("foo", null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with1(null, null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.withoutKey(null);
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
    }


    //=========================================================================
    // Expectations and fixture setup

    private void checkChanges(int changeCount, int delta, Changes changes)
    {
        assertEquals("keys changed", changeCount, changes.changeCount());
        assertEquals("key delta",    delta,       changes.keyCountDelta());
    }


    //=========================================================================
    // Creation

    @Test
    public void compareMutableCreateVsSequential()
    {
        setup(10000);

        FunctionalHashTrie seq = empty();
        for (Map.Entry e : fht)
        {
            seq = seq.with1(e.getKey(), e.getValue());
        }

        assertEquals(fht, seq);
        assertEquals(fht.size(), seq.size());
        for (Map.Entry e : fht)
        {
            Object key = e.getKey();
            assertEquals(fht.get(key), seq.get(key));
        }
    }


    // fromArrays()

    @Test
    public void fromArraysGivenEmptyArraysReturnsEmptySingleton()
    {
        Changes changes = new Changes();

        FunctionalHashTrie t = fromArrays(new Object[0], new Object[0], changes);

        assertSame(empty(), t);
        checkChanges(0, 0, changes);
    }

    @Test(expected = IllegalArgumentException.class)
    public void fromArraysRequiresEqualLengthArrays()
    {
        fromArrays(new Object[] { 1 }, new Object[] { 1, 2 }, new Changes());
    }

    @Test
    public void fromArraysReusesChanges()
    {
        Changes changes = new Changes();

        FunctionalHashTrie t = fromArrays(new Object[] { 1, 2 },
                                          new Object[] { 3, 4 },
                                          changes);

        checkChanges(2, 2, changes);
        assertEquals(3, t.get(1));
        assertEquals(4, t.get(2));

        t = fromArrays(new Object[] { 5, 6, 5 },
                       new Object[] { 8, 9, 0 },
                       changes);

        checkChanges(5, 4, changes);
        assertEquals(0, t.get(5));
        assertEquals(9, t.get(6));
    }


    // fromSelectedKeys()

    @Test
    public void fromSelectedKeysReturnsSubset()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with1(1, 1).with1(2, 2).with1(3, 3);

        FunctionalHashTrie t = fromSelectedKeys(origin, new Object[]{ 1, 3, 5 });

        assertEquals(2,    t.size());
        assertEquals(1,    t.get(1));
        assertEquals(null, t.get(2));
        assertEquals(3,    t.get(3));
    }

    @Test
    public void fromSelectedKeysReturnsEmptySingleton()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with1(1, 1).with1(2, 2);

        FunctionalHashTrie t = fromSelectedKeys(origin, new Object[]{});
        assertSame(FunctionalHashTrie.empty(), t);

        t = fromSelectedKeys(origin, new Object[]{ 3, 4 });
        assertSame(FunctionalHashTrie.empty(), t);
    }


    //=========================================================================
    // Modification

    // with1()

    @Test
    public void testNoopInsertion()
    {
        FunctionalHashTrie trie1 = empty().with1(1, 1);
        FunctionalHashTrie trie2 = trie1.with1(1, 1);
        assertSame(trie1, trie2);
    }

    @Test
    public void testWithReusingChanges()
    {
        FunctionalHashTrie trie = empty();

        Changes changes = new Changes();
        trie = trie.with(1, 1, changes);
        checkChanges(1, 1, changes);
        assertEquals(1, trie.size());

        trie = trie.with(2, 2, changes);
        checkChanges(2, 2, changes);
        assertEquals(2, trie.size());

        trie = trie.with(2, 3, changes);
        checkChanges(3, 2, changes);
        assertEquals(2, trie.size());
    }


    // withoutKeys()

    @Test
    public void withoutKeysGivenKeysReturnsSubset()
    {
        FunctionalHashTrie origin = FunctionalHashTrie.empty().with1(1, 1).with1(2, 2).with1(3, 3);

        FunctionalHashTrie t = origin.withoutKeys(new Object[]{ 1, 3, 5 });

        assertEquals(1,    t.size());
        assertEquals(null, t.get(1));
        assertEquals(2,    t.get(2));
        assertEquals(null, t.get(3));
        assertNotEquals(origin, t);
    }

    @Test
    public void withoutKeysGivenAllKeysReturnsSingleton()
    {
        FunctionalHashTrie origin = empty().with1(1, 1).with1(2, 2);
        FunctionalHashTrie t = origin.withoutKeys(new Object[]{1, 2});
        assertSame(empty(), t);
    }


    // merge1()

    @Test
    public void testMergeInvokesChanges()
    {
        Changes changes = new Changes() {
            @Override
            public Object inserting(Object givenValue)
            {
                return givenValue;
            }

            @Override
            public Object replacing(Object oldValue, Object givenValue)
            {
                assert givenValue.equals("new");
                return oldValue;
            }
        };

        Map.Entry[] entries = { new SimpleEntry<>("f", "old"),
            new SimpleEntry<>("f", "new") };
        FunctionalHashTrie trie = fromEntries(entries, changes);
        assertEquals(1, trie.size());
        assertEquals("old", trie.get("f"));
    }

    @Test
    public void testMergeReusingChanges()
    {
        FunctionalHashTrie empty = empty();

        Changes changes = new Changes();
        FunctionalHashTrie trie1 = empty.with(1, 1, changes);

        // Replace the existing key, so the delta stays the same.
        FunctionalHashTrie trie2 = trie1.merge(empty.with1(1, 2), changes);
        checkChanges(2, 1, changes);
        assertEquals(1, trie2.size());
    }
}
