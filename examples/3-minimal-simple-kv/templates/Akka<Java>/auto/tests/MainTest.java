public class MainTest {
    // TODO: use BehaviorTestKit (https://doc.akka.io/docs/akka/current/typed/testing.html)
    ActorSystem<SpawnProtocol.Command> kvsSystem = null;

    @BeforeEach
    public void setUp() {
        final int numberOfShards = ConfigFactory.load().getInt("akka.cluster.sharding.number-of-shards");
        assertTrue(numberOfShards > 0);
        kvsSystem = ActorSystem.create(KeyValueStoreActorSystem.create(), KeyValueStoreActorSystem.NAME);
    }

    @AfterEach
    public void tearDown() {
        try {
            kvsSystem.terminate();
            kvsSystem.getWhenTerminated().toCompletableFuture().get();
        } catch (InterruptedException | ExecutionException e) {
            fail(e);
        } finally {
            kvsSystem = null;
        }
    }

    @Test
    public void transactionEquality() {
        Transaction t1 = new Transaction(1, null);
        Transaction t2 = new Transaction(1, null);
        Transaction t3 = new Transaction(2, null);

        assertEquals(t1, t2);
        assertNotEquals(t1, t3);
    }

    @Test
    public void testTransactionCoordinator() {
        assertEquals(0, TestActor.nbReceivedReplies);
        // TODO

//        ActorRef<TransactionManagerActor.BeginTransactionReplyEvent> testActor = ActorSystem.create(TestActor.create(), "TestActor");
//        ActorRef<TransactionManagerActor.Command> transactionCoordinatorActor = ActorSystem.create(TransactionManagerActor.create(null), "TransactionCoordinator");
//        transactionCoordinatorActor.tell(new TransactionManagerActor.BeginTransactionEvent(testActor));
//        transactionCoordinatorActor.tell(new TransactionManagerActor.BeginTransactionEvent(testActor));
//
//        try {
//            Thread.sleep(1000);
//        } catch (InterruptedException ignored) {
//        } finally {
//            assertEquals(2, TestActor.nbReceivedReplies);
//        }
    }

    @Test
    public void kvsClientBeginTransactionTest() {
        assertNotNull(kvsSystem);
        KeyValueStoreClient client = new KeyValueStoreClient(kvsSystem);
        try {
            assertEquals(0, client.beginTransaction().id);
            assertEquals(1, client.beginTransaction().id);
            assertEquals(2, client.beginTransaction().id);
        } catch (ExecutionException | InterruptedException e) {
            fail(e);
        } finally {
            kvsSystem.terminate();
        }
    }

    @Test
    public void kvsClientPutGetCommitTest() {
        assertNotNull(kvsSystem);
        KeyValueStoreClient client = new KeyValueStoreClient(kvsSystem);
        try {
            Transaction t = client.beginTransaction();
            assertEquals(0, t.id);

            client.put(t, 0, "hello");
            assertEquals("hello", client.get(t, 0));

            client.put(t, 1, "world");
            assertEquals("world", client.get(t, 1));

            assertTrue(client.commitTransaction(t));

            // has been committed, should still be able to get
            assertEquals("hello", client.get(t, 0));
            assertEquals("world", client.get(t, 1));


            client.put(t, 0, "hi");
            assertEquals("hi", client.get(t, 0));

            assertNull(client.get(t, 2));

        } catch (ExecutionException | InterruptedException e) {
            fail(e);
        } finally {
            kvsSystem.terminate();
        }
    }

    @Test
    public void kvsClientPutGetAbortTest() {
        assertNotNull(kvsSystem);
        KeyValueStoreClient client = new KeyValueStoreClient(kvsSystem);
        try {
            Transaction t = client.beginTransaction();
            assertEquals(0, t.id);

            client.put(t, 0, "hello");
            assertEquals("hello", client.get(t, 0));

            client.put(t, 1, "world");
            assertEquals("world", client.get(t, 1));

            assertTrue(client.abortTransaction(t));     // ABORT HERE

            assertNull(client.get(t, 0));
            assertNull(client.get(t, 1));

            assertFalse(client.commitTransaction(t));    // nothing to commit

            assertNull(client.get(t, 0));
            assertNull(client.get(t, 1));


        } catch (ExecutionException | InterruptedException e) {
            fail(e);
        } finally {
            kvsSystem.terminate();
        }
    } 
}
