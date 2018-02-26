public class ChaoticPlayer extends PcPlayer {
	@Override
	public void doTurn() {
		if(r.nextFloat() <= 0.5)
			stopPlaying();
	}
}
