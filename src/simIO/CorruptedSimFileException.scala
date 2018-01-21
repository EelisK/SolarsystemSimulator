package simIO


/**
 * Thrown when reading a simulation
 * file does not succeed
 * */
class CorruptedSimFileException(message: String) extends Exception(message)