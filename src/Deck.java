import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import exceptions.OutOfCardsException;

public class Deck extends ArrayList<Card> {
	static Deck getNormalDeck() {
		Deck deck = new Deck();
		for(Card.Suit s : Card.Suit.values())
			for(Card.Face f : Card.Face.values())
				deck.add(new Card(f, s));
		return deck;
	}

	private Random r = new Random();
	public Card getRandomCard() throws OutOfCardsException {
		if(size() == 0)
			throw new OutOfCardsException();
		Card c = this.get(r.nextInt(this.size()));
		this.remove(c);
		return c;
	}
}
