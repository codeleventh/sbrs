import java.util.ArrayList;
import java.util.List;

public class Card {
	public enum Suit { HEARTS, CLUBS, DIAMONDS, SPADES }
	public enum Face { TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING, ACE }

	Integer score;
	Suit suit;
	Face face;

	Card(Face f, Suit s) {
		this.suit = s;
		this.face = f;
	}

	public Integer getScore() {
		switch(face) {
			case JACK: case QUEEN: case KING: return 10;
			case ACE: return 11;
			default: return face.ordinal()+2;
		}
	}

	@Override
	public String toString() {
		return face.name() + " of " + suit.name();
	}
}
