import java.util.Random;

public class PcPlayer extends Player {
	Random r = new Random();

	@Override
	public void doTurn() {
		if(getScore() > 16)
			stopPlaying();
		else if(getScore() > 10 && r.nextFloat() >= 0.5)
			stopPlaying();
	}
}
