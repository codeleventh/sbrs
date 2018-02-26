import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public abstract class Player {
	public Boolean inGame = true;

	public String name;
	public Double money = 100d;
	protected Deck cards = new Deck();
	private static Integer id = 0;

	@Override
	protected void finalize() throws Throwable {
		id--;
		super.finalize();
	}
	Player() {
		name = getClass().getSimpleName() + ++id;
	}
	Player(Double money) {
		name = getClass().getSimpleName() + ++id;
	}

	public abstract void doTurn();

	public void reset() {
		cards = new Deck();
		inGame = true;
	}

	public void takeCard(Card card) {
		if(!inGame)
			return;
		cards.add(card);
		System.out.println(name + " got " + card + ". His score now " + getScore() + ".");
		if(getScore() >= 21)
			stopPlaying();
	}

	public Integer getScore() {
		Integer sum = 0, acecount = 0;
		for (Card c : cards) {
			if (c.face != Card.Face.ACE)
				sum += c.getScore();
			else acecount++;
		}
		sum += acecount;

		// нехорошо:
		while (sum + 10 <= 21 && acecount > 0) {
			sum += 10;
			acecount--;
		}
		return sum;
	}

	public void stopPlaying() {
		inGame = false;
		System.out.println(name +" is out of game. ");
	}

	@Override
	public String toString() {
		return name;
	}
}
