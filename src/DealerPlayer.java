import java.util.Random;

public class DealerPlayer extends Player {
	Random r = new Random();

	DealerPlayer() {
		money = Double.POSITIVE_INFINITY;
		name = "Dealer";
	}

	@Override
	public void doTurn() {
		if(getScore() > 17)
			stopPlaying();
	}
}
