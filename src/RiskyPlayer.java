public class RiskyPlayer extends PcPlayer {
	@Override
	public void doTurn() {
		if(getScore() >= 18)
			stopPlaying();
		else if(r.nextFloat() <= 0.4)
			stopPlaying();
	}
}
