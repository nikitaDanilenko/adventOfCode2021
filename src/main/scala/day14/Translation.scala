package day14

object Translation {

  type FrequencyMap = Map[CharKey, BigInt]

  type CharKey = (Char, Char)

  val dictionary: Map[CharKey, Char] = Map(
    ('A', 'A') -> 'A',
    ('B', 'C') -> 'C',
    ('P', 'P') -> 'O',
    ('S', 'K') -> 'K',
    ('K', 'H') -> 'N',
    ('O', 'K') -> 'S',
    ('P', 'C') -> 'O',
    ('V', 'P') -> 'K',
    ('C', 'F') -> 'K',
    ('H', 'C') -> 'H',
    ('F', 'V') -> 'V',
    ('P', 'B') -> 'P',
    ('N', 'K') -> 'H',
    ('C', 'K') -> 'F',
    ('F', 'H') -> 'H',
    ('S', 'V') -> 'B',
    ('N', 'H') -> 'C',
    ('C', 'P') -> 'S',
    ('H', 'P') -> 'O',
    ('H', 'S') -> 'O',
    ('B', 'K') -> 'B',
    ('K', 'C') -> 'P',
    ('V', 'V') -> 'B',
    ('O', 'F') -> 'O',
    ('K', 'P') -> 'V',
    ('F', 'O') -> 'V',
    ('F', 'K') -> 'V',
    ('V', 'H') -> 'K',
    ('K', 'B') -> 'P',
    ('K', 'F') -> 'H',
    ('S', 'H') -> 'S',
    ('H', 'F') -> 'O',
    ('B', 'B') -> 'F',
    ('F', 'C') -> 'O',
    ('S', 'O') -> 'S',
    ('B', 'S') -> 'O',
    ('H', 'H') -> 'C',
    ('B', 'O') -> 'S',
    ('C', 'O') -> 'F',
    ('V', 'C') -> 'V',
    ('K', 'S') -> 'N',
    ('O', 'C') -> 'N',
    ('F', 'P') -> 'P',
    ('H', 'N') -> 'B',
    ('H', 'V') -> 'V',
    ('H', 'O') -> 'P',
    ('K', 'O') -> 'C',
    ('S', 'F') -> 'H',
    ('N', 'O') -> 'N',
    ('P', 'S') -> 'C',
    ('B', 'P') -> 'K',
    ('S', 'C') -> 'C',
    ('N', 'P') -> 'C',
    ('C', 'H') -> 'V',
    ('K', 'V') -> 'B',
    ('H', 'K') -> 'V',
    ('O', 'P') -> 'V',
    ('S', 'P') -> 'V',
    ('N', 'C') -> 'V',
    ('F', 'F') -> 'B',
    ('C', 'C') -> 'V',
    ('C', 'S') -> 'F',
    ('S', 'B') -> 'C',
    ('O', 'S') -> 'C',
    ('F', 'N') -> 'O',
    ('C', 'V') -> 'P',
    ('O', 'H') -> 'H',
    ('O', 'O') -> 'P',
    ('P', 'O') -> 'F',
    ('N', 'S') -> 'H',
    ('V', 'B') -> 'K',
    ('O', 'V') -> 'K',
    ('P', 'H') -> 'H',
    ('B', 'H') -> 'V',
    ('S', 'S') -> 'B',
    ('P', 'K') -> 'F',
    ('V', 'K') -> 'O',
    ('B', 'N') -> 'V',
    ('V', 'F') -> 'O',
    ('P', 'F') -> 'H',
    ('V', 'S') -> 'K',
    ('O', 'N') -> 'V',
    ('B', 'F') -> 'F',
    ('C', 'N') -> 'F',
    ('V', 'O') -> 'B',
    ('F', 'S') -> 'K',
    ('O', 'B') -> 'B',
    ('P', 'N') -> 'H',
    ('N', 'F') -> 'O',
    ('V', 'N') -> 'P',
    ('B', 'V') -> 'S',
    ('N', 'V') -> 'V',
    ('F', 'B') -> 'V',
    ('N', 'B') -> 'P',
    ('C', 'B') -> 'B',
    ('K', 'K') -> 'S',
    ('N', 'N') -> 'F',
    ('S', 'N') -> 'B',
    ('H', 'B') -> 'P',
    ('P', 'V') -> 'S',
    ('K', 'N') -> 'S'
  )

  val dict2 = Map(
    ('C', 'H') -> 'B',
    ('H', 'H') -> 'N',
    ('C', 'B') -> 'H',
    ('N', 'H') -> 'C',
    ('H', 'B') -> 'C',
    ('H', 'C') -> 'B',
    ('H', 'N') -> 'C',
    ('N', 'N') -> 'C',
    ('B', 'H') -> 'H',
    ('N', 'C') -> 'B',
    ('N', 'B') -> 'B',
    ('B', 'N') -> 'B',
    ('B', 'B') -> 'N',
    ('B', 'C') -> 'B',
    ('C', 'C') -> 'N',
    ('C', 'N') -> 'C'
  )

}