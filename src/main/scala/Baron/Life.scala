package Baron

object Life {
    type Position = (Int, Int)
    type ConwayState = Map[Position, Boolean]

  /**
    * Reads if the position given in a state is alive or not.
    * @param p the Position of type (Int, Int),
    * @param s the state.
    * @return Returns a boolean, true if alive, false if not.
    */ //CHECKS IF ALIVE
    def isAlive(p:Position, s:ConwayState):Boolean = s.getOrElse(p,false)

  /**
    * Blinker states for testing purposes.
    */ //BLINKERS
    val blinker1:ConwayState = Map(
      (2, 1) -> true, (2, 2) -> true, (2, 3) -> true
    )
    val blinker2:ConwayState = Map(
      (1, 2) -> true, (2, 2) -> true, (3, 2) -> true
    )

  /**
    * I've programmed Game of Life (and more general automata of other Rules) before in several languages,
    * but never been sure of the most efficient way to check neighbours. This is what I've got, and I don't
    * like it. It seems very inefficient to use a double for loop for just 8 values. Any other suggestions
    * would be appreciated. Considered trying to use
    *
    * neighbours += true match{
    * case isAlive(pos1,state) => 1
    * case isAlive(pos2,state) => 1
    * .
    * .
    * .
    * }
    *
    * but don't think it works syntactically to list a function call as a case. Would basically be just a
    * weirdly implemented for loop anyway; This is probably better.
    */ //NOTE
  /**
    * This function checks the live neighbours of a Position in a given state.
    * @param pos The Position.
    * @param state The state.
    * @return Returns an Int giving the number of neighbours.
    */ //CHECKS # LIVE NEIGHBOURS
    def liveNeighbours(pos:Position, state:ConwayState):Int = {
      var neighNum:Int = 0
      for (i <- -1 to 1)
        for (j <- -1 to 1){                                         //Small double for loop to scout neighbours
          if (!(i==0&j==0)){                                        //Excludes self
            if (isAlive((pos._1+i,pos._2+j),state)){                //Checks neighbour based on position in for loop
              neighNum = neighNum+1                                 //Adds one if there's a neighbour there
            }
          }
        }
      neighNum
    }

  /**
    * Checks if the given position in the given state is alive next turn, based on liveNeighbours.
    * @param pos The position.
    * @param state The state.
    * @return Boolean, true if alive, false if not.
    */ //CHECKS IF POSITION IS ALIVE NEXT TURN
    def aliveNextTurn(pos:(Int, Int), state:ConwayState):Boolean = {
      var live: Boolean = false                                     //Assume false until proven otherwise, as it's more common
      if (isAlive(pos, state) & liveNeighbours(pos, state) == 2)    //Alive + 2 Neighbours -> Alive
        live = true else
      if (liveNeighbours(pos, state) == 3)                          //3 Neighbours -> Alive regardless of current state
        live = true
      live
  }

  /**
    * Iterates over a 2D grid to check if each Position is alive in the next state, records those, returns the state.
    * @param state The current state.
    * @param maxSize The dimensions to check.
    * @return Returns a ConwayState that is the successor to the in-state.
    */ //PROCESSES TO NEXT STATE
    def nextConwayState(state:ConwayState, maxSize:(Int, Int) = (120, 100)):ConwayState = {
      var nextState: ConwayState = Map()                          //New, empty state
      for (i <- 0 to maxSize._1) {
        for (j <- 0 to maxSize._2) {                              //Double loop over the possible coordinates
          val myPos: Position = (i, j)                            //List coordinates as a position for ease
          if (aliveNextTurn(myPos, state))                        //Checks each coordinate
            nextState = nextState.updated(myPos, true)            //Populates state as needed
        }
      }
    nextState
    }
}
