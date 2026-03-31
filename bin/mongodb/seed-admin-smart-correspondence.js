/**
 * Dev seed: user `admin` gets 5 synthetic analysed correspondence games in
 * puzzle2_smart_recent (Smart Puzzles time-control bucket).
 *
 * Run: mongosh lichess bin/mongodb/seed-admin-smart-correspondence.js
 *
 * Requires user4 _id "admin" (spamdb creates this by default).
 */
const uid = 'admin';
const u = db.user4.findOne({ _id: uid });
if (!u) {
  print('No user4 with _id "' + uid + '". Create it first (e.g. spamdb or signup).');
  quit(1);
}

const now = new Date();
const daysAgo = (n) => new Date(now.getTime() - n * 86400000);

/** @param {number} ply @param {string} uci @param {string} j @param {number} c @param {number} pc @param {number} np */
const mv = (ply, uci, j, c, pc, np) => ({ p: ply, u: uci, j: j, c: c, pc: pc, np: np });

/** @param {string} gi @param {Date} t @param {boolean} uw @param {object[]} moves */
const game = (gi, t, uw, moves) => ({
  gi: gi,
  a: t,
  pt: t,
  uw: uw,
  m: moves,
});

// Newest first (matches indexer merge order).
const correspondenceGames = [
  game(
    'smcrsp01',
    daysAgo(1),
    true,
    [
      mv(10, 'e2e4', 'Inaccuracy', -25, 32, 10),
      mv(12, 'g1f3', 'Mistake', -90, 32, 10),
    ],
  ),
  game(
    'smcrsp02',
    daysAgo(3),
    false,
    [
      mv(11, 'e7e5', 'Blunder', -220, 32, 10),
      mv(15, 'b8c6', 'Inaccuracy', -40, 32, 10),
    ],
  ),
  game(
    'smcrsp03',
    daysAgo(5),
    true,
    [mv(14, 'f1c4', 'Mistake', -75, 30, 9), mv(18, 'e1g1', 'Inaccuracy', -20, 30, 9)],
  ),
  game(
    'smcrsp04',
    daysAgo(8),
    false,
    [mv(20, 'd7d5', 'Blunder', -300, 28, 8)],
  ),
  game(
    'smcrsp05',
    daysAgo(12),
    true,
    [
      mv(8, 'd2d4', 'Inaccuracy', -15, 32, 10),
      mv(16, 'c1g5', 'Mistake', -110, 30, 9),
    ],
  ),
];

db.puzzle2_smart_recent.updateOne(
  { _id: uid },
  {
    $set: {
      bs: { correspondence: correspondenceGames },
      ua: now,
    },
  },
  { upsert: true },
);

print('Seeded puzzle2_smart_recent for ' + uid + ': correspondence count = ' + correspondenceGames.length);
